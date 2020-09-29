package qova.logic;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import qova.enums.CourseFaculty;
import qova.enums.CourseType;
import qova.forms.CourseForm;
import qova.forms.InstanceTitleForm;
import qova.objects.Course;
import qova.objects.CourseInstance;
import qova.objects.SurveyResponse;
import qova.repositories.BinaryResponseRepository;
import qova.repositories.CourseInstanceRepository;
import qova.repositories.CourseRepository;
import qova.repositories.MultipleChoiceResponseRepository;
import qova.repositories.SingleChoiceResponseRepository;
import qova.repositories.SurveyResponseRepository;
import qova.repositories.TextResponseRepository;

import com.google.zxing.BarcodeFormat;
import com.google.zxing.WriterException;
import com.google.zxing.client.j2se.MatrixToImageWriter;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.qrcode.QRCodeWriter;

import javax.transaction.Transactional;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

@Service
@Transactional
public class CourseManagement {

    private final CourseRepository coursesRepo;
    private final CourseInstanceRepository courseInstancesRepo;
    private final SurveyResponseRepository surveyResponseRepository;
    private final BinaryResponseRepository binaryResponseRepository;
    private final TextResponseRepository textResponseRepository;
    private final SingleChoiceResponseRepository singleChoiceResponseRepository;
    private final MultipleChoiceResponseRepository multipleChoiceResponseRepository;

    @Autowired
    public CourseManagement(CourseRepository coursesRepo, CourseInstanceRepository courseInstancesRepo,
            SurveyResponseRepository surveyResponseRepository, BinaryResponseRepository binaryResponseRepository,
            TextResponseRepository textResponseRepository,
            SingleChoiceResponseRepository singleChoiceResponseRepository,
            MultipleChoiceResponseRepository multipleChoiceResponseRepository) {

        this.surveyResponseRepository = Objects.requireNonNull(surveyResponseRepository);
        this.binaryResponseRepository = Objects.requireNonNull(binaryResponseRepository);
        this.textResponseRepository = Objects.requireNonNull(textResponseRepository);
        this.singleChoiceResponseRepository = Objects.requireNonNull(singleChoiceResponseRepository);
        this.multipleChoiceResponseRepository = Objects.requireNonNull(multipleChoiceResponseRepository);
        this.coursesRepo = Objects.requireNonNull(coursesRepo);
        this.courseInstancesRepo = Objects.requireNonNull(courseInstancesRepo);
    }

    // Create Course and get Id from new course
    public UUID createCourseReturnId(CourseForm form) {
        Objects.requireNonNull(form);

        // Form attributes
        var name = form.getName();
        var semesterOfStudents = form.getSemesterOfStudents();
        var faculty = form.getFaculty();
        var courseDate = parseSemesterString(form.getSemesterString());
        var semesterString = form.getSemesterString();

        // create CourseInstances
        Map<CourseType, CourseInstance> courseInstances = createCourseInstances(form);

        Course crs = new Course(name, courseInstances.get(CourseType.LECTURE), courseInstances.get(CourseType.TUTORIAL),
                courseInstances.get(CourseType.SEMINAR), courseInstances.get(CourseType.PRACTICAL), semesterOfStudents,
                faculty, semesterString, courseDate);
        coursesRepo.save(crs);

        return crs.getId();
    }

    // Method for createing CourseInstances

    private Map<CourseType, CourseInstance> createCourseInstances(CourseForm form) {

        // Map containing CourseInstances, with CourseType as key
        EnumMap<CourseType, CourseInstance> courseInstances = new EnumMap<>(CourseType.class);

        for (CourseType courseType : CourseType.values()) {
            if (Boolean.TRUE.equals(form.getInstanceExists(courseType))) {

                // In the current implementation, the groupAmount for LECTURE is always set to 1
                // (The attribute exists in case of a future need for this to be editable. It
                // would have to be bound to the form in the frontend, and called here with
                // form.getGroupAmountLecture)
                Integer groupAmount;

                if (courseType.equals(CourseType.LECTURE)) {
                    groupAmount = 1;
                } else {
                    groupAmount = form.getGroupAmount(courseType);
                }

                // Initialise the instanceTitles array with the amount of instances that are set
                // to exist
                List<String> instanceTitles = new ArrayList<>();
                for (int i = 0; i < form.getInstanceAmount(courseType); i++) {
                    instanceTitles.add("");
                }

                // Create the courseInstance
                CourseInstance instance = new CourseInstance(CourseType.LECTURE, groupAmount,
                        form.getInstanceAmount(courseType), instanceTitles, true);

                // save to database
                courseInstancesRepo.save(instance);

                // Add CourseInstance to map
                courseInstances.put(courseType, instance);
            } else {
                CourseInstance instance = new CourseInstance(courseType);
                courseInstancesRepo.save(instance);
                courseInstances.put(courseType, instance);
            }
        }

        // Return the Map of CourseInstances
        return courseInstances;
    }

    // update course details
    public void updateCourseDetails(UUID id, CourseForm form) {

        Optional<Course> crs = coursesRepo.findById(id);
        if (crs.isPresent()) {

            // The existing course object being edited
            Course course = crs.get();

            // These attributes are always editable!
            course.setName(form.getName());
            course.setSemesterOfStudents(form.getSemesterOfStudents());
            course.setFaculty(form.getFaculty());

            // These attributes are NOT editable, when the instance has been finalised!!!

            for (CourseType courseType : CourseType.values()) {
                // Lecture EXISTS, but is toggled OFF
                if (Boolean.TRUE.equals(course.getInstanceExists(courseType))
                        && Boolean.FALSE.equals(form.getInstanceExists(courseType))) {

                    course.getInstance(courseType).setInactive();
                }
                // Lecture does NOT EXIST, but is toggled ON
                if (Boolean.FALSE.equals(course.getInstanceExists(courseType))
                        && Boolean.TRUE.equals(form.getInstanceExists(courseType))) {

                    // Initialise instanceTitles array
                    List<String> instanceTitles = new ArrayList<>();
                    for (int i = 0; i < form.getInstanceAmount(courseType); i++) {
                        instanceTitles.add("");
                    }

                    // Update CourseInstance
                    CourseInstance instance = course.getInstance(courseType);

                    // Set to one for lectures (in case of change, assign
                    // form.getLectureGroupAmount)
                    if (courseType.equals(CourseType.LECTURE)) {
                        instance.setGroupAmount(1);
                    } else {
                        instance.setGroupAmount(form.getGroupAmount(courseType));
                    }

                    //
                    instance.setInstanceAmount(form.getInstanceAmount(courseType));
                    instance.setInstanceTitles(instanceTitles);
                    instance.setActive();
                }
            }

            // We are intentionally not allowing the option to edit the CourseDate of
            // SemesterString
        }
    }

    // Set Instance titles for each CourseInstance
    public void createCourseSetInstanceTitles(InstanceTitleForm form, UUID id) {
        Optional<Course> crs = coursesRepo.findById(id);
        if (crs.isPresent()) {
            Course course = crs.get();

            for (CourseType courseType : CourseType.values()) {

                if (Boolean.TRUE.equals(course.getInstanceExists(courseType))) {
                    course.getInstance(courseType).setInstanceTitles(form.getInstanceTitlesForType(courseType));
                }
            }
        }
    }

    // Gets the relevant Survey in the course objects, based on the given surveyType
    public String getSurveyforType(UUID id, String type) {
        Optional<Course> crs = coursesRepo.findById(id);
        if (crs.isPresent()) {
            Course course = crs.get();
            if (type.equals("LECTURE")) {
                return course.getLecture().getSurvey();
            } else if (type.equals("TUTORIAL")) {
                return course.getTutorial().getSurvey();
            } else if (type.equals("SEMINAR")) {
                return course.getSeminar().getSurvey();
            } else if (type.equals("PRACTICAL")) {
                return course.getPractical().getSurvey();
            }
        }
        return "Something went wrong";
    }

    // Sets the relevant Survey in the course objects, based on the given surveyType
    public void setSurveyforType(Course course, String type, String survey) {

        if (type.equals("LECTURE")) {
            course.getLecture().setSurvey(survey);
        } else if (type.equals("TUTORIAL")) {
            course.getTutorial().setSurvey(survey);
        } else if (type.equals("SEMINAR")) {
            course.getSeminar().setSurvey(survey);
        } else if (type.equals("PRACTICAL")) {
            course.getPractical().setSurvey(survey);
        }
    }

    public Course duplicateCourse(UUID id, String semesterString) {

        Optional<Course> crs = findById(id);
        if (crs.isPresent()) {

            Course oldCourse = crs.get();

            Course newCourse = new Course(oldCourse.getName(), duplicateCourseInstance(oldCourse.getLecture()),
                    duplicateCourseInstance(oldCourse.getTutorial()), duplicateCourseInstance(oldCourse.getSeminar()),
                    duplicateCourseInstance(oldCourse.getPractical()), oldCourse.getSemesterOfStudents(),
                    oldCourse.getFaculty(), semesterString, parseSemesterString(semesterString));

            coursesRepo.save(newCourse);

            return newCourse;
        }

        return null;
    }

    public CourseInstance duplicateCourseInstance(CourseInstance oldInstance) {
        List<String> newInstanceTitles = new ArrayList<>();
        for (String title : oldInstance.getInstanceTitles()) {
            newInstanceTitles.add(title);
        }
        CourseInstance newInstance = new CourseInstance(oldInstance.getCourseType(), oldInstance.getGroupAmount(),
                oldInstance.getInstanceAmount(), newInstanceTitles, oldInstance.isActive());
        courseInstancesRepo.save(newInstance);
        return newInstance;
    }

    /**
     * QR-Code Generator
     * 
     * @param text Takes a string as input (in our case a url)
     * @return A byte[] with the image of the QRCode
     * @throws WriterException thrown by QRCode generator
     * @throws IOException     thrown by QRCode generator
     */
    public byte[] generateQRCodeImage(String text) throws WriterException, IOException {

        // configure width and height
        int height = 1500;
        int width = 1500;

        QRCodeWriter qrCodeWriter = new QRCodeWriter();
        BitMatrix bitMatrix = qrCodeWriter.encode(text, BarcodeFormat.QR_CODE, width, height);

        ByteArrayOutputStream pngOutputStream = new ByteArrayOutputStream();
        MatrixToImageWriter.writeToStream(bitMatrix, "PNG", pngOutputStream);
        return pngOutputStream.toByteArray();

    }

    // Generates a set amount of semesters which are added to the model, to pick
    // from as course creation dates.
    /**
     * Function used to populate a drop down menu in course creation UI. Fills a
     * list with (x) future semesters and (y) previous semsters, as well as the
     * current semester. {@linkplain Course} only has a LocalDate attribute, so
     * parseSemesterString method in controller converts this string back to a date,
     * which is used for finding courses by date
     * 
     * @return ArrayList with strings of type: "SoSe xxxx" or "WiSe xxxx/yyyy"
     */
    public ArrayList<String> findSemesters() {

        // Current Date
        LocalDate dateNow = LocalDate.now();

        // Current year and month as ints
        int currentYear = dateNow.getYear();
        int currentMonth = dateNow.getMonthValue();

        // List sent to controller
        ArrayList<String> semesters = new ArrayList<>();

        // Summer semester start is April(4), winter semester starts in October(10)
        // Winter semester spans over the new year, so it is of the format WiSe xx/yy

        // if winter semester and in year xx
        if (currentMonth < 4) {

            // previous semesters
            semesters.add("WiSe " + String.valueOf(currentYear - 2) + "/" + String.valueOf(currentYear - 1));
            semesters.add("SoSe " + String.valueOf(currentYear - 1));

            // current Semester
            semesters.add("WiSe " + String.valueOf(currentYear - 1) + "/" + String.valueOf(currentYear));

            // future semesters
            semesters.add("SoSe " + String.valueOf(currentYear));
            semesters.add("WiSe " + String.valueOf(currentYear) + "/" + String.valueOf(currentYear + 1));
        }

        // if winter semster and in year yy
        else if (currentMonth >= 10) {
            // previous semesters
            semesters.add("WiSe " + String.valueOf(currentYear - 1 + "/" + String.valueOf(currentYear)));
            semesters.add("SoSe " + String.valueOf(currentYear));

            // current Semester
            semesters.add("WiSe " + String.valueOf(currentYear) + "/" + String.valueOf(currentYear + 1));

            // future semesters
            semesters.add("SoSe " + String.valueOf(currentYear + 1));
            semesters.add("WiSe " + String.valueOf(currentYear + 1) + "/" + String.valueOf(currentYear + 2));
        }

        // if summer semester
        else {
            // previous semesters
            semesters.add("SoSe " + String.valueOf(currentYear - 1));
            semesters.add("WiSe " + String.valueOf(currentYear - 1) + "/" + String.valueOf(currentYear));

            // current Semester
            semesters.add("SoSe " + String.valueOf(currentYear));

            // future semesters
            semesters.add("WiSe " + String.valueOf(currentYear) + "/" + String.valueOf(currentYear + 1));
            semesters.add("SoSe " + String.valueOf(currentYear + 1));
        }

        // return arraylist
        return semesters;
    }

    // Parse Semester String and convert to date
    public LocalDate parseSemesterString(String semString) {

        // Split string at space
        String[] tokens = semString.split(" ");

        int year;

        // If SoSe
        if (tokens[0].equals("SoSe")) {

            // try to parse the string for the int of the year
            try {
                year = Integer.parseInt(tokens[1]);
            }

            // else set year to 00000
            catch (NumberFormatException e) {
                year = 0000;
            }

            // return date
            return LocalDate.of(year, 4, 1);
        }

        // If WiSe
        else if (tokens[0].equals("WiSe")) {

            // String is of form: "WiSe xxxx/yyyy", so we split at "/"
            String[] yearX = tokens[1].split("/");

            // Try to parse the year (the year xxxx from "xxxx/yyyy") from string. We choose
            // xxxx because this makes sorting easier later
            try {
                year = Integer.parseInt(yearX[0]);
            }

            // else set year to 0000
            catch (NumberFormatException e) {
                year = 0000;
            }

            // return Date
            return LocalDate.of(year, 10, 1);
        }

        else { // TODO: what to do when wrong date is entered?
            return LocalDate.of(0, 1, 1);
        }
    }

    /**
     * @param id the Course id
     * @return an {@linkplain Optional} of a {@linkplain Course} with the given id
     */
    public Optional<Course> findById(UUID id) {
        return coursesRepo.findById(id);
    }

    /**
     * @return an {@linkplain Iterable} of a {@linkplain Course} with the given id
     */
    public Iterable<Course> findAll() {
        return coursesRepo.findAll();
    }

    // delete course
    public void deleteCourse(UUID id) {
        Optional<Course> crs = findById(id);
        if (crs.isPresent()) {
            Course course = crs.get();
            deleteCourseInstancesForCourse(course);
            deleteSurveyResponseAndAsscoiatedResponses(course);
        }
        coursesRepo.deleteById(id);
    }

    /**
     * Delete {@linkplain qova.objects.CourseInstance}s for a given
     * {@linkplain qova.objects.Course} Object
     * 
     * @param course {@linkplain qova.objects.Course}
     */
    public void deleteCourseInstancesForCourse(Course course) {
        for (CourseType type : CourseType.values()) {
            courseInstancesRepo.delete(course.getInstance(type));
        }
    }

    /**
     * Deletes all objects of type {@linkplain qova.objects.SurveyResponse} and its
     * subtypes {@linkplain qova.objects.BinaryResponse},
     * {@linkplain qova.objects.TextResponse},
     * {@linkplain qova.objects.SingleChoiceResponse},
     * {@linkplain qova.objects.MultipleChoiceResponse}
     * 
     * @param course {@linkplain qova.objects.Course}
     */
    public void deleteSurveyResponseAndAsscoiatedResponses(Course course) {

        // Get all surveyResponses for a course
        Iterable<SurveyResponse> surveyResponses = surveyResponseRepository.findByCourse(course);

        // delete all instances
        for (SurveyResponse r : surveyResponses) {
            binaryResponseRepository.deleteAll(binaryResponseRepository.findBySurveyResponse(r));
            textResponseRepository.deleteAll(textResponseRepository.findBySurveyResponse(r));
            singleChoiceResponseRepository.deleteAll(singleChoiceResponseRepository.findBySurveyResponse(r));
            multipleChoiceResponseRepository.deleteAll(multipleChoiceResponseRepository.findBySurveyResponse(r));
        }

        // delete the surveyresponses
        surveyResponseRepository.deleteAll(surveyResponses);
    }

    // Test Methods
    // TODO: Remove Before Production

    // Test Method, remove in final build
    public void TestCreateCourse() {
        var name = "Rechnernetze";

        List<String> lectureTitles = new ArrayList<>();
        lectureTitles.addAll(
                Arrays.asList("Einführung", "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2",
                        "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance",
                        "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"));
        var lecture = new CourseInstance(CourseType.LECTURE, 1, 12, lectureTitles, true);

        courseInstancesRepo.save(lecture);

        List<String> tutorialTitles = new ArrayList<>();
        lectureTitles.addAll(
                Arrays.asList("Einführung", "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2",
                        "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance",
                        "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"));
        var tutorial = new CourseInstance(CourseType.TUTORIAL, 8, 12, tutorialTitles, true);
        tutorial.setSurvey(
                "[{\"type\":\"SingleChoice\",\"question\":\"Hat die Übung Wissen vermittelt, welches du dir nicht im Selbststudium hättest erarbeiten können?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Hat der/die Leiter/in den aktiven Austausch mit den Studierenden gesucht?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Waren die Anforderung dem Wissensstand der Studierenden angemessen?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Konnte die Übung gezielt Schwerpunkte setzen und Struktur vermitteln?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Konnte der/die Leiter/in dein Interesse an dem Thema wecken?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Hat der/die Leiter/in die Möglichkeiten einer Übung gegenüber der Vorlesung ausgeschöpft?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Online Lehre v.s. Präsenzveranstaltung\",\"answers\":[\"Die Übung war digital und soll digital bleiben.\",\"Die Übung war digital und wäre als Präsenzveranstaltung besser.\",\"Die Übung war eine Präsenzveranstaltung und soll eine bleiben.\",\"Die Übung war eine Präsenzveranstaltung und sollte digital werden.\"]}]");

        courseInstancesRepo.save(tutorial);

        List<String> seminarTitles = new ArrayList<>();
        lectureTitles.addAll(
                Arrays.asList("Einführung", "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2",
                        "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance",
                        "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"));
        var seminar = new CourseInstance(CourseType.SEMINAR, 8, 12, seminarTitles, true);

        courseInstancesRepo.save(seminar);

        List<String> practicalTitles = new ArrayList<>();
        lectureTitles.addAll(
                Arrays.asList("Einführung", "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2",
                        "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance",
                        "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"));
        var practical = new CourseInstance(CourseType.PRACTICAL, 8, 12, practicalTitles, true);

        courseInstancesRepo.save(practical);

        var semesterOfStudents = 4;
        var faculty = CourseFaculty.COMPUTER_SCIENCE;
        var courseDate = LocalDate.of(2020, 10, 4);
        var semesterString = "SoSe 2020";

        coursesRepo.save(new Course(name, lecture, tutorial, seminar, practical, semesterOfStudents, faculty,
                semesterString, courseDate));
    }

    public Course TimTestCreateCourse() {

        List<String> lectureTitles = new ArrayList<>();
        lectureTitles.addAll(
                Arrays.asList("Einführung", "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2",
                        "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance",
                        "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"));
        var lecture = new CourseInstance(CourseType.LECTURE, 1, 11, lectureTitles, true);
        List<String> tutorialTitles = new ArrayList<>();
        tutorialTitles.addAll(
                Arrays.asList("Einführung", "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2",
                        "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance",
                        "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"));
        var tutorial = new CourseInstance(CourseType.TUTORIAL, 2, 12, tutorialTitles, true);
        List<String> seminarTitles = new ArrayList<>();
        seminarTitles.addAll(
                Arrays.asList("Einführung", "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2",
                        "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance",
                        "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"));
        var seminar = new CourseInstance(CourseType.SEMINAR, 3, 13, seminarTitles, true);
        List<String> practicalTitles = new ArrayList<>();
        practicalTitles.addAll(
                Arrays.asList("Einführung", "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2",
                        "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance",
                        "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"));
        var practical = new CourseInstance(CourseType.TUTORIAL, 4, 14, practicalTitles, true);

        var name = "Rechnernetze";
        var semesterOfStudents = 4;
        var faculty = CourseFaculty.COMPUTER_SCIENCE;
        var courseDate = LocalDate.of(2020, 10, 4);
        var semesterString = "SoSe 2020";

        return new Course(name, lecture, tutorial, seminar, practical, semesterOfStudents, faculty, semesterString,
                courseDate);
    }

}
