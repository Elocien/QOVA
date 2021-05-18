package qova.logic;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;

import qova.admin.DefaultSurvey;
import qova.enums.CourseFaculty;
import qova.enums.CourseType;
import qova.forms.CourseForm;
import qova.forms.InstanceTitleForm;
import qova.objects.Course;
import qova.objects.CourseInstance;
import qova.objects.SurveyResponse;
import qova.repositories.AbstractResponseRepository;
import qova.repositories.CourseInstanceRepository;
import qova.repositories.CourseRepository;
import qova.repositories.SurveyResponseRepository;

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
    private final AbstractResponseRepository abstractResponseRepository;

    @Autowired
    public CourseManagement(CourseRepository coursesRepo, CourseInstanceRepository courseInstancesRepo,
                            SurveyResponseRepository surveyResponseRepository, AbstractResponseRepository abstractResponseRepository) {

        this.surveyResponseRepository = Objects.requireNonNull(surveyResponseRepository);
        this.abstractResponseRepository = Objects.requireNonNull(abstractResponseRepository);
        this.coursesRepo = Objects.requireNonNull(coursesRepo);
        this.courseInstancesRepo = Objects.requireNonNull(courseInstancesRepo);
    }

    // Create Course and get Id from new course
    public UUID createCourseAndCourseInstanceAndReturnCourseId(String userId, CourseForm form, EnumMap<CourseType, DefaultSurvey> defaultSurveyMap) {
        Objects.requireNonNull(form);

        // Form attributes
        var name = form.getName();
        var semesterOfStudents = form.getSemesterOfStudents();
        var faculty = form.getFaculty();
        var courseDate = parseSemesterString(form.getSemesterString());
        var semesterString = form.getSemesterString();

        // Create CourseInstances
        Map<CourseType, CourseInstance> courseInstances = createCourseInstances(form, defaultSurveyMap);

        // Create the Course
        Course crs = new Course(name, userId, courseInstances.get(CourseType.LECTURE), courseInstances.get(CourseType.TUTORIAL),
                courseInstances.get(CourseType.SEMINAR), courseInstances.get(CourseType.PRACTICAL), semesterOfStudents,
                faculty, semesterString, courseDate);
        coursesRepo.save(crs);

        return crs.getId();
    }

    // Method for createing CourseInstances

    private Map<CourseType, CourseInstance> createCourseInstances(CourseForm form, EnumMap<CourseType, DefaultSurvey> defaultSurveyMap) {

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
                }
                else if(form.getGroupAmount(courseType).equals(0)){
                    groupAmount = 1;
                }
                else {
                    groupAmount = form.getGroupAmount(courseType);
                }

                Integer instanceAmount;
                if(form.getInstanceAmount(courseType).equals(0)){
                    instanceAmount = 1;
                }
                else {
                    instanceAmount = form.getInstanceAmount(courseType);
                }


                // Initialise the instanceTitles array with the amount of instances that are set
                // to exist
                List<String> instanceTitles = new ArrayList<>();
                for (int i = 0; i < form.getInstanceAmount(courseType); i++) {
                    instanceTitles.add("");
                }

                // Create the courseInstance
                CourseInstance instance = new CourseInstance(courseType, groupAmount,
                        instanceAmount, instanceTitles, defaultSurveyMap.get(courseType));

                // save to database
                courseInstancesRepo.save(instance);

                // Add CourseInstance to map
                courseInstances.put(courseType, instance);
            } else {
                CourseInstance instance = new CourseInstance(courseType, defaultSurveyMap.get(courseType));
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
            course.setSemesterString(form.getSemesterString());

            // These attributes are NOT editable, when the instance has been finalised!!!

            for (CourseType courseType : CourseType.values()) {
                // Instance EXISTS, but is toggled OFF
                if (Boolean.TRUE.equals(course.getInstanceExists(courseType))
                        && Boolean.FALSE.equals(form.getInstanceExists(courseType))) {

                    course.getInstance(courseType).setInactive();
                }
                // Instance does NOT EXIST, but is toggled ON
                else if (Boolean.FALSE.equals(course.getInstanceExists(courseType))
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
                // The Instance hasn't been activated or deactivate, but has been edited
                else {
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
                    CourseInstance instance = course.getInstance(courseType);
                    instance.setInstanceTitles(form.getInstanceTitlesForType(courseType));
                    instance.setInstanceAmount(form.getInstanceTitlesForType(courseType).size());
                }
            }
        }
    }

    // Gets the relevant Survey in the course objects, based on the given surveyType
    public String getSurveyforType(UUID id, CourseType type) {
        Optional<Course> crs = coursesRepo.findById(id);
        if (crs.isPresent()) {
            Course course = crs.get();

            switch (type) {
                case LECTURE:
                    return course.getLecture().getSurvey();
                case TUTORIAL:
                    return course.getTutorial().getSurvey();
                case SEMINAR:
                    return course.getSeminar().getSurvey();
                case PRACTICAL:
                    return course.getPractical().getSurvey();
                default:
                    return "[]";
            }
        }

        return "[]";
    }

    // Sets the relevant Survey in the course objects, based on the given surveyType
    public void setSurveyforType(Course course, String type, String survey) {
        switch (type) {
            case "LECTURE":
                course.getLecture().setSurvey(survey);
                break;
            case "TUTORIAL":
                course.getTutorial().setSurvey(survey);
                break;
            case "SEMINAR":
                course.getSeminar().setSurvey(survey);
                break;
            case "PRACTICAL":
                course.getPractical().setSurvey(survey);
                break;
        }
    }

    /**
     * Duplicates a {@linkplain Course}, only differing from the individual in the given <b>Semester</b>
     *
     * @param id The id of the original {@linkplain Course}
     * @param semesterString The new Semester set by the course owner, in the form of a String
     * @return A new Course and all of its instances
     */
    public Course duplicateCourse(UUID id, String semesterString) {

        Optional<Course> crs = findById(id);
        if (crs.isPresent()) {

            Course oldCourse = crs.get();

            Course newCourse = new Course(oldCourse.getName(), oldCourse.getOwnerId(), duplicateCourseInstance(oldCourse.getLecture()),
                    duplicateCourseInstance(oldCourse.getTutorial()), duplicateCourseInstance(oldCourse.getSeminar()),
                    duplicateCourseInstance(oldCourse.getPractical()), oldCourse.getSemesterOfStudents(),
                    oldCourse.getFaculty(), semesterString, parseSemesterString(semesterString));

            coursesRepo.save(newCourse);

            return newCourse;
        }

        return null;
    }

    public CourseInstance duplicateCourseInstance(CourseInstance oldInstance) {
        List<String> newInstanceTitles = new ArrayList<>(oldInstance.getInstanceTitles());
        CourseInstance newInstance = new CourseInstance(oldInstance.getCourseType(), oldInstance.getGroupAmount(),
                oldInstance.getInstanceAmount(), newInstanceTitles, oldInstance.getDefaultSurvey());
        courseInstancesRepo.save(newInstance);
        return newInstance;
    }


    public Integer getNumberOfSurveysMissing(Course course){
        int numberOfSurveyMissing = 0;
        for (CourseType courseType : CourseType.values()){
            if (course.getInstanceExists(courseType) && !course.getInstance(courseType).getSurveyEditedFlag()) {
                numberOfSurveyMissing ++;
            }
        }
        return numberOfSurveyMissing;
    }

    public Boolean getInstanceTitlesMissingFlag(Course course){
        for (CourseType courseType : CourseType.values()){
            var instance = course.getInstance(courseType);
            if (instance.isActive() && instance.titlesMissing()) {
                return true;
            }
        }
        return false;
    }

    /**
     * Used to set the finalisedFalg attribute of a
     * {@linkplain qova.objects.Course}.
     *
     * @param id the {@link java.util.UUID} of the {@linkplain qova.objects.Course}
     */
    public void setCourseFinalised(UUID id) {
        Optional<Course> crs = findById(id);

        crs.ifPresent(Course::setCourseAsFinalised);
    }

    /**
     * Used to set the surveyEditedFlag attribute of a
     * {@linkplain qova.objects.CourseInstance}.
     *
     * @param courseInstance The {@linkplain qova.objects.CourseInstance} for which
     *                       the flag is to be set
     */
    public void setSurveyEditedFlagForCourseInstance(CourseInstance courseInstance) {
        courseInstance.setSurveEditedFlag();
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

    public HttpEntity<byte[]> qrCodeAsHttpEntity(String type, UUID id) throws IOException, WriterException {

        //The url contained in the QR Code. This redirects to the survey selection screen
        String url = "qova.med.tu-dresden.de/survey/select?type=" + type + "&id=" + id+"&mode=participant";

        //Retrieve the relevant course
        Optional<Course> crs = findById(id);

        //Generate the filename of the qrcode image
        String filename = crs.get().getName() + "_" + type + "_" + "QRCode";

        // Generate QRCode
        byte[] qrcode = generateQRCodeImage(url);

        // Set HTTP headers and return HttpEntity
        HttpHeaders header = new HttpHeaders();
        header.setContentType(MediaType.IMAGE_PNG);
        header.set(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + filename);
        header.setContentLength(qrcode.length);

        return new HttpEntity<>(qrcode, header);
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
    public List<String> findSemesters() {

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
            semesters.add("SoSe " + (currentYear - 1));

            // current Semester
            semesters.add("WiSe " + (currentYear - 1) + "/" + (currentYear));

            // future semesters
            semesters.add("SoSe " + (currentYear));
            semesters.add("WiSe " + (currentYear) + "/" + (currentYear + 1));
            semesters.add("SoSe " + (currentYear + 1));
        }

        // if winter semster and in year yy
        else if (currentMonth >= 10) {
            // previous semesters
            semesters.add("SoSe " + (currentYear));

            // current Semester
            semesters.add("WiSe " + (currentYear) + "/" + (currentYear + 1));

            // future semesters
            semesters.add("SoSe " + (currentYear + 1));
            semesters.add("WiSe " + (currentYear + 1) + "/" + (currentYear + 2));
            semesters.add("SoSe " + (currentYear + 2));
        }

        // if summer semester
        else {
            // previous semesters
            semesters.add("WiSe " + (currentYear - 1) + "/" + (currentYear));

            // current Semester
            semesters.add("SoSe " + (currentYear));

            // future semesters
            semesters.add("WiSe " + (currentYear) + "/" + (currentYear + 1));
            semesters.add("SoSe " + (currentYear + 1));
            semesters.add("WiSe " + (currentYear + 1) + "/" + (currentYear + 2));
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
                year = 0;
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
                year = 0;
            }

            // return Date
            return LocalDate.of(year, 10, 1);
        } else { 
            return LocalDate.of(0, 1, 1);
        }
    }

    /**
     * Find a {@linkplain Course} with the given Id
     *
     * @param id the Course id
     * @return an {@linkplain Optional} of a {@linkplain Course} with the given id
     */
    public Optional<Course> findById(UUID id) {
        return coursesRepo.findById(id);
    }

    /**
     * Retrieves all {@linkplain Course}s from the repository
     *
     * @return an {@linkplain Iterable} of all {@linkplain Course}s
     */
    public Iterable<Course> findAll() {
        return coursesRepo.findAll();
    }

    /**
     * Deletes a {@linkplain Course} with the given Id
     *
     * @param id The Id of the Course
     */
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
     * Finds all {@linkplain Course}s based on the ownerId field
     *
     * @param id the Course id
     * @return an {@linkplain Optional} of a {@linkplain Course} with the given id
     */
    public Iterable<Course> findByOwnerid(String id) {
        return coursesRepo.findByOwnerId(id);
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

        for (SurveyResponse surveyResponse : surveyResponses)
            // delete all instances
            abstractResponseRepository.deleteAll(surveyResponse.getListOfResponses());

        // delete the surveyresponses
        surveyResponseRepository.deleteAll(surveyResponses);
    }

    /**
     * Find a {@linkplain Course} with the given Id
     *
     * @param courseId The Course id
     * @param ownerId  The UserId of the courseOwner
     * @return an {@linkplain Optional} of a {@linkplain Course} with the given id
     */
    public Boolean findIfUserOwnsCourse(UUID courseId, String ownerId) {
        Optional<Course> crs = coursesRepo.findByIdAndOwnerId(courseId, ownerId);
        return crs.isPresent();
    }

}
