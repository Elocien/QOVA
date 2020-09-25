package qova.logic;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import qova.enums.CourseFaculty;
import qova.enums.CourseType;
import qova.forms.CourseForm;
import qova.forms.InstanceTitleForm;
import qova.objects.Course;
import qova.objects.CourseInstance;
import qova.repositories.CourseInstanceRepository;
import qova.repositories.CourseRepository;

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
import java.util.EnumMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

@Service
@Transactional
public class CourseManagement {

    private final CourseRepository coursesRepo;
    private final CourseInstanceRepository courseInstancesRepo;

    @Autowired
    public CourseManagement(CourseRepository coursesRepo, CourseInstanceRepository courseInstancesRepo) {
        this.coursesRepo = Objects.requireNonNull(coursesRepo);
        this.courseInstancesRepo = Objects.requireNonNull(courseInstancesRepo);
    }





    //Create Course and get Id from new course
    public UUID createCourseReturnId(CourseForm form) {
        Objects.requireNonNull(form);

        //Form attributes
        var name = form.getName();
        var semesterOfStudents = form.getSemesterOfStudents();
        var faculty = form.getFaculty();
        var courseDate = parseSemesterString(form.getSemesterString());
        var semesterString = form.getSemesterString();


        //create CourseInstances
        Map<CourseType, CourseInstance> courseInstances = createCourseInstance(form);


        Course crs  = new Course(name, courseInstances.get(CourseType.LECTURE), courseInstances.get(CourseType.TUTORIAL), courseInstances.get(CourseType.SEMINAR), courseInstances.get(CourseType.PRACTICAL), semesterOfStudents, faculty, semesterString, courseDate);
        coursesRepo.save(crs);
        
        return crs.getId();
    }



    //Method for createing CourseInstances

    private Map<CourseType, CourseInstance> createCourseInstance(CourseForm form) {

        //Map containing CourseInstances, with CourseType as key
        EnumMap<CourseType, CourseInstance> courseInstances = new EnumMap<>(CourseType.class);

        //Create CourseInstances if bool is true 
        //LECTURE
        if(Boolean.TRUE.equals(form.getLectureExists())){

            //In the current implementation, the groupAmount for LECTURE is always set to 1
            //(The attribute exists in case of a future need for this to be editable. It would have to be bound to the form in the frontend, and called here with form.getGroupAmountLecture)
            var groupAmount = 1; 

            //Initialise the instanceTitles array with the amount of instances that are set to exist
            String[] instanceTitles = new String[form.getInstanceAmountLecture()];

            //Create the courseInstance
            CourseInstance lecture = new CourseInstance(CourseType.LECTURE, groupAmount, form.getInstanceAmountLecture(), instanceTitles, true);

            //save to database 
            courseInstancesRepo.save(lecture);

            //Add CourseInstance to map
            courseInstances.put(lecture.getCourseType(), lecture);
        }
        else{
            CourseInstance lecture = new CourseInstance(CourseType.LECTURE);
            courseInstances.put(lecture.getCourseType(), lecture);
        }


        //TUTORIAL
        if(Boolean.TRUE.equals(form.getTutorialExists())){

            //Initialise the instanceTitles array with the amount of instances that are set to exist
            String[] instanceTitles = new String[form.getInstanceAmountLecture()];

            //Create the courseInstance
            CourseInstance tutorial = new CourseInstance(CourseType.TUTORIAL, form.getGroupAmountPractical(), form.getInstanceAmountLecture(), instanceTitles, true);

            //save to database 
            courseInstancesRepo.save(tutorial);

            //Add CourseInstance to map
            courseInstances.put(tutorial.getCourseType(), tutorial);
        }
        else{
            CourseInstance tutorial = new CourseInstance(CourseType.TUTORIAL);
            courseInstances.put(tutorial.getCourseType(), tutorial);
        }


        //SEMINAR
        if(Boolean.TRUE.equals(form.getSeminarExists())){

            //Initialise the instanceTitles array with the amount of instances that are set to exist
            String[] instanceTitles = new String[form.getInstanceAmountLecture()];

            //Create the courseInstance
            CourseInstance seminar = new CourseInstance(CourseType.SEMINAR, form.getGroupAmountPractical(), form.getInstanceAmountLecture(), instanceTitles, true);

            //save to database 
            courseInstancesRepo.save(seminar);

            //Add CourseInstance to map
            courseInstances.put(seminar.getCourseType(), seminar);
        }
        else{
            CourseInstance seminar = new CourseInstance(CourseType.SEMINAR);
            courseInstances.put(seminar.getCourseType(), seminar);
        }


        //PRACTICAL
        if(Boolean.TRUE.equals(form.getPracticalExists())){

            //Initialise the instanceTitles array with the amount of instances that are set to exist
            String[] instanceTitles = new String[form.getInstanceAmountLecture()];

            //Create the courseInstance
            CourseInstance practical = new CourseInstance(CourseType.PRACTICAL, form.getGroupAmountPractical(), form.getInstanceAmountLecture(), instanceTitles, true);

            //save to database 
            courseInstancesRepo.save(practical);

            //Add CourseInstance to map
            courseInstances.put(practical.getCourseType(), practical);
        }
        else{
            CourseInstance practical = new CourseInstance(CourseType.PRACTICAL);
            courseInstances.put(practical.getCourseType(), practical);
        }

        //Return the Map of CourseInstances
        return courseInstances;
    }









    //update course details
    public void updateCourseDetails(UUID id, CourseForm form) {

        Optional<Course> crs = coursesRepo.findById(id);
        if (crs.isPresent()){

            //The existing course object being edited
            Course course = crs.get();

            //These attributes are always editable!
            course.setName(form.getName());
            course.setSemesterOfStudents(form.getSemesterOfStudents());
            course.setFaculty(form.getFaculty());


            //These attributes are NOT editable, when the instance has been finalised!!!


            //Only execute if lecture is not finalised
            if(Boolean.FALSE.equals(course.getLecture().isFinalised())){

                //Lecture EXISTS, but is toggled OFF
                if(Boolean.TRUE.equals(course.getLectureExists()) && Boolean.FALSE.equals(form.getLectureExists())){

                    course.getLecture().setInactive();
                }
                //Lecture does NOT EXIST, but is toggled ON
                if(Boolean.FALSE.equals(course.getLectureExists()) && Boolean.TRUE.equals(form.getLectureExists())){

                    //Initialise instanceTitles array
                    String[] instanceTitles = new String[form.getInstanceAmountLecture()];

                    //Update CourseInstance
                    CourseInstance lecture = course.getLecture();

                    //Set to one for lectures (in case of change, assign form.getLectureGroupAmount)
                    lecture.setGroupAmount(1);

                    //
                    lecture.setInstanceAmount(form.getInstanceAmountLecture());
                    lecture.setInstanceTitles(instanceTitles);
                    lecture.setActive();
                }
            }

            //Only execute if tutorial is not finalised
            if(Boolean.FALSE.equals(course.getTutorial().isFinalised())){

                //tutorial EXISTS, but is toggled OFF
                if(Boolean.TRUE.equals(course.getTutorialExists()) && Boolean.FALSE.equals(form.getTutorialExists())){

                    course.getTutorial().setInactive();
                }
                //tutorial does NOT EXIST, but is toggled ON
                if(Boolean.FALSE.equals(course.getTutorialExists()) && Boolean.TRUE.equals(form.getTutorialExists())){

                    //Initialise instanceTitles array
                    String[] instanceTitles = new String[form.getInstanceAmountTutorial()];

                    //Update CourseInstance
                    CourseInstance tutorial = course.getTutorial();

                    //
                    tutorial.setGroupAmount(form.getGroupAmountTutorial());
                    tutorial.setInstanceAmount(form.getInstanceAmountTutorial());
                    tutorial.setInstanceTitles(instanceTitles);
                    tutorial.setActive();
                }
            }


            //Only execute if seminar is not finalised
            if(Boolean.FALSE.equals(course.getSeminar().isFinalised())){

                //seminar EXISTS, but is toggled OFF
                if(Boolean.TRUE.equals(course.getSeminarExists()) && Boolean.FALSE.equals(form.getSeminarExists())){

                    course.getSeminar().setInactive();
                }
                //seminar does NOT EXIST, but is toggled ON
                if(Boolean.FALSE.equals(course.getSeminarExists()) && Boolean.TRUE.equals(form.getSeminarExists())){

                    //Initialise instanceTitles array
                    String[] instanceTitles = new String[form.getInstanceAmountSeminar()];

                    //Update CourseInstance
                    CourseInstance seminar = course.getSeminar();

                    //
                    seminar.setGroupAmount(form.getGroupAmountSeminar());
                    seminar.setInstanceAmount(form.getInstanceAmountSeminar());
                    seminar.setInstanceTitles(instanceTitles);
                    seminar.setActive();
                }
            }



            //Only execute if practical is not finalised
            if(Boolean.FALSE.equals(course.getPractical().isFinalised())){

                //practical EXISTS, but is toggled OFF
                if(Boolean.TRUE.equals(course.getPracticalExists()) && Boolean.FALSE.equals(form.getPracticalExists())){

                    course.getPractical().setInactive();
                }
                //practical does NOT EXIST, but is toggled ON
                if(Boolean.FALSE.equals(course.getPracticalExists()) && Boolean.TRUE.equals(form.getPracticalExists())){

                    //Initialise instanceTitles array
                    String[] instanceTitles = new String[form.getInstanceAmountPractical()];

                    //Update CourseInstance
                    CourseInstance practical = course.getPractical();

                    //
                    practical.setGroupAmount(form.getGroupAmountPractical());
                    practical.setInstanceAmount(form.getInstanceAmountPractical());
                    practical.setInstanceTitles(instanceTitles);
                    practical.setActive();
                }
            }


            //We are intentionally not allowing the option to edit the CourseDate of SemesterString
        }
    }




    //Set Instance titles for each CourseInstance
    public void createCourseSetInstanceTitles(InstanceTitleForm form, UUID id){
        Optional<Course> crs = coursesRepo.findById(id);
        if(crs.isPresent()){
            Course course = crs.get();

            if(Boolean.TRUE.equals(course.getLectureExists())){
                course.getLecture().setInstanceTitles(form.getLectureInstanceTitles());
            }

            if(Boolean.TRUE.equals(course.getTutorialExists())){
                course.getTutorial().setInstanceTitles(form.getTutorialInstanceTitles());
            }

            if(Boolean.TRUE.equals(course.getSeminarExists())){
                course.getSeminar().setInstanceTitles(form.getSeminarInstanceTitles());
            }

            if(Boolean.TRUE.equals(course.getPracticalExists())){
                course.getPractical().setInstanceTitles(form.getPracticalInstanceTitles());
            }
        }
    }

    
    


    //Gets the relevant Survey in the course objects, based on the given surveyType
    public String getSurveyforType (UUID id, String type){
        Optional<Course> crs = coursesRepo.findById(id);
        if (crs.isPresent()){
            Course course = crs.get();
            if (type.equals("LECTURE")){
                return course.getLecture().getSurvey();
            }
            else if (type.equals("TUTORIAL")){
                return course.getTutorial().getSurvey();
            }
            else if (type.equals("SEMINAR")){
                return course.getSeminar().getSurvey();
            }
            else if (type.equals("PRACTICAL")){
                return course.getPractical().getSurvey();
            }
        }
        return "Something went wrong";
    }



    //Sets the relevant Survey in the course objects, based on the given surveyType
    public void setSurveyforType (Course course, String type, String survey){ 

        if(type.equals("LECTURE")) {
            course.getLecture().setSurvey(survey);
        }
        else if(type.equals("TUTORIAL")) {
            course.getTutorial().setSurvey(survey);
        }
        else if (type.equals("SEMINAR")){
            course.getSeminar().setSurvey(survey);
        }
        else if (type.equals("PRACTICAL")){
            course.getPractical().setSurvey(survey);
        }
    }





    public Course duplicateCourse(UUID id, String semesterString){
    
        Optional<Course> crs = findById(id);
        if(crs.isPresent()){

            Course oldCourse = crs.get();
            
            Course newCourse = new Course(oldCourse.getName(), duplicateCourseInstance(oldCourse.getLecture()), duplicateCourseInstance(oldCourse.getTutorial()), 
                duplicateCourseInstance(oldCourse.getSeminar()), duplicateCourseInstance(oldCourse.getPractical()), oldCourse.getSemesterOfStudents(), 
                oldCourse.getFaculty(), semesterString, parseSemesterString(semesterString));
            
            coursesRepo.save(newCourse);

            return newCourse;
        }

        return null;
    }


    public CourseInstance duplicateCourseInstance(CourseInstance oldInstance){
        return new CourseInstance(oldInstance.getCourseType(), oldInstance.getGroupAmount(), oldInstance.getInstanceAmount(), oldInstance.getInstanceTitles(), oldInstance.isActive());
    }











    /**
     * QR-Code Generator
     * 
     * @param text Takes a string as input (in our case a url)
     * @return A byte[] with the image of the QRCode
     * @throws WriterException thrown by QRCode generator
     * @throws IOException thrown by QRCode generator
     */
    public byte[] generateQRCodeImage(String text) throws WriterException, IOException {

        //configure width and height
        int height = 1500;
        int width = 1500;


        QRCodeWriter qrCodeWriter = new QRCodeWriter();
        BitMatrix bitMatrix = qrCodeWriter.encode(text, BarcodeFormat.QR_CODE, width, height);

        ByteArrayOutputStream pngOutputStream = new ByteArrayOutputStream();
        MatrixToImageWriter.writeToStream(bitMatrix, "PNG", pngOutputStream);
        return pngOutputStream.toByteArray(); 
        
    }





    //Generates a set amount of semesters which are added to the model, to pick from as course creation dates.
    /**
     * Function used to populate a drop down menu in course creation UI. Fills a list with (x) future semesters and (y) previous semsters, as well as the current semester. {@linkplain Course}
     * only has a LocalDate attribute, so parseSemesterString method in controller converts this string back to a date, which is used for finding courses by date
     * @return ArrayList with strings of type: "SoSe xxxx" or "WiSe xxxx/yyyy"
     */
    public ArrayList<String> findSemesters(){
        
        //Current Date
        LocalDate dateNow = LocalDate.now();

        //Current year and month as ints
        int currentYear = dateNow.getYear();
        int currentMonth = dateNow.getMonthValue();



        //List sent to controller
        ArrayList<String> semesters = new ArrayList<>();


        //Summer semester start is April(4), winter semester starts in October(10)
        //Winter semester spans over the new year, so it is of the format WiSe xx/yy


        //if winter semester and in year xx
        if(currentMonth <4){
            
            //previous semesters  
            semesters.add("WiSe " + String.valueOf(currentYear- 2) + "/" + String.valueOf(currentYear- 1));
            semesters.add("SoSe " + String.valueOf(currentYear -1));

            //current Semester
            semesters.add("WiSe " + String.valueOf(currentYear-1) + "/" + String.valueOf(currentYear));

            //future semesters
            semesters.add("SoSe " + String.valueOf(currentYear));
            semesters.add("WiSe " + String.valueOf(currentYear) + "/" + String.valueOf(currentYear + 1));
        }
        
        //if winter semster and in year yy
        else if(currentMonth >= 10){
            //previous semesters
            semesters.add("WiSe " + String.valueOf(currentYear- 1 + "/" + String.valueOf(currentYear)));
            semesters.add("SoSe " + String.valueOf(currentYear));

            //current Semester
            semesters.add("WiSe " + String.valueOf(currentYear) + "/" + String.valueOf(currentYear + 1));

            //future semesters
            semesters.add("SoSe " + String.valueOf(currentYear + 1));
            semesters.add("WiSe " + String.valueOf(currentYear + 1) + "/" + String.valueOf(currentYear + 2));
        }


        //if summer semester
        else{
            //previous semesters
            semesters.add("SoSe " + String.valueOf(currentYear - 1));
            semesters.add("WiSe " + String.valueOf(currentYear - 1) + "/" + String.valueOf(currentYear));

            //current Semester
            semesters.add("SoSe " + String.valueOf(currentYear));

            //future semesters
            semesters.add("WiSe " + String.valueOf(currentYear) + "/" + String.valueOf(currentYear + 1));
            semesters.add("SoSe " + String.valueOf(currentYear + 1));
        }
        

        //return arraylist
        return semesters;
    }






    //Parse Semester String and convert to date
    public LocalDate parseSemesterString(String semString){   
        
        //Split string at space
        String[] tokens = semString.split(" ");

        int year;

        //If SoSe
        if(tokens[0].equals("SoSe")){
            
            //try to parse the string for the int of the year
            try {year = Integer.parseInt(tokens[1]);}

            //else set year to 00000
            catch (NumberFormatException e){year = 0000;}

            //return date
            return LocalDate.of(year, 4, 1);
        }


        //If WiSe
        else if(tokens[0].equals("WiSe")){
            
            //String is of form: "WiSe xxxx/yyyy", so we split at "/"
            String[] yearX = tokens[1].split("/");


            //Try to parse the year (the year xxxx from "xxxx/yyyy") from string. We choose xxxx because this makes sorting easier later
            try {year = Integer.parseInt(yearX[0]);}

            //else set year to 0000
            catch (NumberFormatException e){year = 0000;}

            //return Date
            return LocalDate.of(year, 10, 1);
        }

        else{   //TODO: what to do when wrong date is entered?
            return LocalDate.of(0, 1, 1);
        }
    }


    /**
	 * @param id the Course id
	 * @return an {@linkplain Optional} of a {@linkplain Course}
	 *         with the given id
	 */
	public Optional<Course> findById(UUID id) {
		return coursesRepo.findById(id);
	}


    /**
	 * @return an {@linkplain Iterable} of a {@linkplain Course}
	 *         with the given id
	 */
	public Iterable<Course> findAll() {
		return coursesRepo.findAll();
    }
    
    //delete course
    public void deleteCourse(UUID id) {
        coursesRepo.deleteById(id);
    }

    //delete course
    public void deleteCourseInstancesForCourse(UUID id) {
        Optional<Course> crs = findById(id);
        if(crs.isPresent()){
            Course course = crs.get();
            courseInstancesRepo.delete(course.getLecture());
            courseInstancesRepo.delete(course.getTutorial());
            courseInstancesRepo.delete(course.getSeminar());
            courseInstancesRepo.delete(course.getPractical());
        }
    }









































    

    //Test Methods
    //TODO: Remove Before Production







    //Test Method, remove in final build
    public void TestCreateCourse() {
        var name = "Rechnernetze";

        String[] lectureTitles = {"Einführung" , "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2", "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance", "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"};
        var lecture = new CourseInstance(CourseType.LECTURE, 1, 12, lectureTitles, true);

        courseInstancesRepo.save(lecture);

        String[] tutorialTitles = {"Einführung" , "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2", "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance", "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"};
        var tutorial = new CourseInstance(CourseType.TUTORIAL, 8, 12, tutorialTitles, true);
        tutorial.setSurvey("[{\"type\":\"SingleChoice\",\"question\":\"Hat die Übung Wissen vermittelt, welches du dir nicht im Selbststudium hättest erarbeiten können?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Hat der/die Leiter/in den aktiven Austausch mit den Studierenden gesucht?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Waren die Anforderung dem Wissensstand der Studierenden angemessen?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Konnte die Übung gezielt Schwerpunkte setzen und Struktur vermitteln?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Konnte der/die Leiter/in dein Interesse an dem Thema wecken?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Hat der/die Leiter/in die Möglichkeiten einer Übung gegenüber der Vorlesung ausgeschöpft?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Online Lehre v.s. Präsenzveranstaltung\",\"answers\":[\"Die Übung war digital und soll digital bleiben.\",\"Die Übung war digital und wäre als Präsenzveranstaltung besser.\",\"Die Übung war eine Präsenzveranstaltung und soll eine bleiben.\",\"Die Übung war eine Präsenzveranstaltung und sollte digital werden.\"]}]");

        courseInstancesRepo.save(tutorial);

        String[] seminarTitles = {"Einführung" , "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2", "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance", "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"};
        var seminar = new CourseInstance(CourseType.SEMINAR, 8, 12, seminarTitles, true);

        courseInstancesRepo.save(seminar);

        String[] pTitles = {"Einführung" , "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2", "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance", "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"};
        var practical = new CourseInstance(CourseType.PRACTICAL, 8, 12, pTitles, true);

        courseInstancesRepo.save(practical);

        var semesterOfStudents = 4;
        var faculty = CourseFaculty.COMPUTER_SCIENCE;
        var courseDate = LocalDate.of(2020, 10, 4);
        var semesterString = "SoSe 2020";

        coursesRepo.save(new Course(name, lecture, tutorial, seminar, practical, semesterOfStudents, faculty, semesterString, courseDate));
    }

    public Course TimTestCreateCourse() {

        String[] lectureTitles = {"Einführung" , "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2", "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance", "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"};
        var lecture = new CourseInstance(CourseType.LECTURE, 1, 11, lectureTitles, true);
        String[] tutorialTitles = {"Einführung" , "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2", "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance", "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"};
        var tutorial = new CourseInstance(CourseType.TUTORIAL, 2, 12, tutorialTitles, true);
        String[] seminarTitles = {"Einführung" , "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2", "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance", "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"};
        var seminar = new CourseInstance(CourseType.SEMINAR, 3, 13, seminarTitles, true);
        String[] pTitles = {"Einführung" , "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2", "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance", "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"};
        var practical = new CourseInstance(CourseType.TUTORIAL, 4, 14, pTitles, true);

        var name = "Rechnernetze";
        var semesterOfStudents = 4;
        var faculty = CourseFaculty.COMPUTER_SCIENCE;
        var courseDate = LocalDate.of(2020, 10, 4);
        var semesterString = "SoSe 2020";

        return new Course(name, lecture, tutorial, seminar, practical, semesterOfStudents, faculty, semesterString, courseDate);
    }


}
