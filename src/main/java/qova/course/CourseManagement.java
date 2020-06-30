package qova.course;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

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
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

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
    public String createCourseReturnId(CourseForm form) {
        Objects.requireNonNull(form);

        //Name of Course
        var name = form.getName();

        //create CourseInstances
        Map<CourseType, CourseInstance> courseInstances = createCourseInstance(form);


        var semesterOfStudents = form.getSemesterOfStudents();
        var faculty = form.getFaculty();
        var courseDate = parseSemesterString(form.getSemesterString());
        var semesterString = form.getSemesterString();

        Course crs  = new Course(name, courseInstances.get(CourseType.LECTURE), courseInstances.get(CourseType.TUTORIAL), courseInstances.get(CourseType.SEMINAR), courseInstances.get(CourseType.PRACTICAL), semesterOfStudents, faculty, semesterString, courseDate);
        coursesRepo.save(crs);
        
        return crs.getId();
    }



    //Method for createing CourseInstances
    private Map<CourseType, CourseInstance> createCourseInstance(CourseForm form) {

        //Map containing CourseInstances, with CourseType as key
        Map<CourseType, CourseInstance> courseInstances = new HashMap<CourseType, CourseInstance>();

        //Create CourseInstances if bool is true 
        //LECTURE
        if(form.getLectureExists()){

            //In the current implementation, the groupAmount for LECTURE is always set to 1
            //(The attribute exists in case of future need for this to be editable. It would have to be bound to the form in the frontend, and called here with form.getGroupAmountLecture)
            var groupAmount = 1; 

            //Initialise the instanceTitles array with the amount of instances that are set to exist
            String[] instanceTitles = new String[form.getInstanceAmountLecture()];

            //Create the courseInstance
            CourseInstance lecture = new CourseInstance(CourseType.LECTURE, groupAmount, form.getInstanceAmountLecture(), instanceTitles);

            //save to database 
            courseInstancesRepo.save(lecture);

            //Add CourseInstance to map
            courseInstances.put(CourseType.LECTURE, lecture);
        }
        else{courseInstances.put(CourseType.LECTURE, null);}


        //TUTORIAL
        if(form.getTutorialExists()){

            //Initialise the instanceTitles array with the amount of instances that are set to exist
            String[] instanceTitles = new String[form.getInstanceAmountLecture()];

            //Create the courseInstance
            CourseInstance tutorial = new CourseInstance(CourseType.TUTORIAL, form.getGroupAmountPractical(), form.getInstanceAmountLecture(), instanceTitles);

            //save to database 
            courseInstancesRepo.save(tutorial);

            //Add CourseInstance to map
            courseInstances.put(CourseType.TUTORIAL, tutorial);
        }
        else{courseInstances.put(CourseType.TUTORIAL, null);}


        //SEMINAR
        if(form.getSeminarExists()){

            //Initialise the instanceTitles array with the amount of instances that are set to exist
            String[] instanceTitles = new String[form.getInstanceAmountLecture()];

            //Create the courseInstance
            CourseInstance seminar = new CourseInstance(CourseType.SEMINAR, form.getGroupAmountPractical(), form.getInstanceAmountLecture(), instanceTitles);

            //save to database 
            courseInstancesRepo.save(seminar);

            //Add CourseInstance to map
            courseInstances.put(CourseType.SEMINAR, seminar);
        }
        else{courseInstances.put(CourseType.SEMINAR, null);}


        //PRACTICAL
        if(form.getPracticalExists()){

            //Initialise the instanceTitles array with the amount of instances that are set to exist
            String[] instanceTitles = new String[form.getInstanceAmountLecture()];

            //Create the courseInstance
            CourseInstance practical = new CourseInstance(CourseType.PRACTICAL, form.getGroupAmountPractical(), form.getInstanceAmountLecture(), instanceTitles);

            //save to database 
            courseInstancesRepo.save(practical);

            //Add CourseInstance to map
            courseInstances.put(CourseType.PRACTICAL, practical);
        }
        else{courseInstances.put(CourseType.PRACTICAL, null);} 

        return courseInstances;
    }





    
    //delete course
    public void deleteCourse(String id) {
        coursesRepo.deleteById(id);
    }



    //update course details
    public void updateCourseDetails(String id, CourseForm form) {
        Optional<Course> crs = coursesRepo.findById(id);
        if (crs.isPresent()){

            //The existing course object being edited
            Course course = crs.get();

            //name
            course.setName(form.getName());


            //Lecture EXISTS, but is toggled OFF
            if(course.getLectureExists() && !form.getLectureExists()){
                course.setLecture(null);
            }
            //Lecture does NOT EXIST, but is toggled ON
            if(!course.getLectureExists() && form.getLectureExists()){
                //Set to one for lectures (in case of change, assign form.getLectureGroupAmount)
                Integer groupAmount = 1;

                //Initialise instanceTitles array
                String[] instanceTitles = new String[form.getInstanceAmountLecture()];

                //Create CourseInstance
                CourseInstance lecture = new CourseInstance(CourseType.LECTURE, groupAmount, form.getInstanceAmountLecture(), instanceTitles);

                //Add CourseInstance to Course
                course.setLecture(lecture);

                //Save new CourseInstance to database
                courseInstancesRepo.save(lecture);
            }


            //Tutorial EXISTS, but is toggled OFF
            if(course.getTutorialExists() && !form.getTutorialExists()){
                course.setTutorial(null);
            }
            //Tutorial does NOT EXIST, but is toggled ON
            if(!course.getTutorialExists() && form.getTutorialExists()){

                //Initialise instanceTitles array
                String[] instanceTitles = new String[form.getInstanceAmountTutorial()];

                //Create CourseInstance
                CourseInstance tutorial = new CourseInstance(CourseType.TUTORIAL, form.getGroupAmountTutorial(), form.getInstanceAmountTutorial(), instanceTitles);

                //Add CourseInstance to Course
                course.setTutorial(tutorial);

                //Save new CourseInstance to database
                courseInstancesRepo.save(tutorial);
            }


            //Seminar EXISTS, but is toggled OFF
            if(course.getSeminarExists() && !form.getSeminarExists()){
                course.setSeminar(null);
            }
            //Seminar does NOT EXIST, but is toggled ON
            if(!course.getSeminarExists() && form.getSeminarExists()){

                //Initialise instanceTitles array
                String[] instanceTitles = new String[form.getInstanceAmountSeminar()];

                //Create CourseInstance
                CourseInstance seminar = new CourseInstance(CourseType.SEMINAR, form.getGroupAmountSeminar(), form.getInstanceAmountSeminar(), instanceTitles);

                //Add CourseInstance to Course
                course.setSeminar(seminar);

                //Save new CourseInstance to database
                courseInstancesRepo.save(seminar);
            }


            //Practical EXISTS, but is toggled OFF
            if(course.getPracticalExists() && !form.getPracticalExists()){
                course.setPractical(null);
            }
            //Practical does NOT EXIST, but is toggled ON
            if(!course.getPracticalExists() && form.getPracticalExists()){
                
                //Initialise instanceTitles array
                String[] instanceTitles = new String[form.getInstanceAmountPractical()];

                //Create CourseInstance
                CourseInstance practical = new CourseInstance(CourseType.PRACTICAL, form.getGroupAmountPractical(), form.getInstanceAmountPractical(), instanceTitles);

                //Add CourseInstance to Course
                course.setPractical(practical);

                //Save new CourseInstance to database
                courseInstancesRepo.save(practical);
            }

            course.setSemesterOfStudents(form.getSemesterOfStudents());
            course.setFaculty(form.getFaculty());

            //We are intentionally not allowing the option to edit the CourseDate of SemesterString
        }
    }




    //Set Instance titles for each CourseInstance
    public void createCourseSetInstanceTitles(InstanceTitleForm form, String id){
        Optional<Course> crs = coursesRepo.findById(id);
        if(crs.isPresent()){
            Course course = crs.get();

            if(course.getLectureExists()){
                course.getLecture().setInstanceTitles(form.getLectureInstanceTitles());
            }

            if(course.getTutorialExists()){
                course.getTutorial().setInstanceTitles(form.getTutorialInstanceTitles());
            }

            if(course.getSeminarExists()){
                course.getSeminar().setInstanceTitles(form.getSeminarInstanceTitles());
            }

            if(course.getPracticalExists()){
                course.getPractical().setInstanceTitles(form.getPracticalInstanceTitles());
            }
        }
    }



    //Gets the relevant Survey in the course objects, based on the given surveyType
    public String getSurveyforType (String id, String type){
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
    public void setSurveyforType (String id, String type, SurveyForm form){
        Optional<Course> crs = coursesRepo.findById(id);
        if (crs.isPresent()){

            Course course = crs.get();

            //if CourseType is Lecture, then save Survey as lectureSurvey
            if(type.equals("LECTURE")) {
                course.getLecture().setSurvey(form.getQuestionnairejson());
            }
            else if(type.equals("TUTORIAL")) {
                course.getTutorial().setSurvey(form.getQuestionnairejson());
            }
            else if (type.equals("SEMINAR")){
                course.getSeminar().setSurvey(form.getQuestionnairejson());
            }
            else if (type.equals("PRACTICAL")){
                course.getPractical().setSurvey(form.getQuestionnairejson());
            }
        }
    }





    /**
     * QR-Code Generator
     * 
     * @param text Takes a string as input (in our case a url)
     * @return A byte[] with the image of the QRCode
     * @throws WriterException
     * @throws IOException
     */
    public byte[] generateQRCodeImage(String text) throws WriterException, IOException {

        //configure width and height
        int height = 1500;
        int width = 1500;


        QRCodeWriter qrCodeWriter = new QRCodeWriter();
        BitMatrix bitMatrix = qrCodeWriter.encode(text, BarcodeFormat.QR_CODE, width, height);

        ByteArrayOutputStream pngOutputStream = new ByteArrayOutputStream();
        MatrixToImageWriter.writeToStream(bitMatrix, "PNG", pngOutputStream);
        byte[] pngData = pngOutputStream.toByteArray(); 
        return pngData;
    }





    //Generates a set amount of semesters which are added to the model, to pick from as course creation dates.
    /**
     * Function used to populate a drop down menu in course creation UI. Fills a list with (x) future semesters and (y) previous semsters, as well as the current semester. {@linkplain Course}
     * only has a LocalDate attribute, so parseSemesterString method in controller converts this string back to a date, which is used for finding courses by date
     * @return ArrayList<String> with strings of type: "SoSe xxxx" or "WiSe xxxx/yyyy"
     */
    public ArrayList<String> findSemesters(){
        
        //Current Date
        LocalDate dateNow = LocalDate.now();

        //Current year and month as ints
        int currentYear = dateNow.getYear();
        int currentMonth = dateNow.getMonthValue();



        //List sent to controller
        ArrayList<String> semesters = new ArrayList<String>();


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
	public Optional<Course> findById(String id) {
		return coursesRepo.findById(id);
	}


    /**
	 * @return an {@linkplain Iterable} of a {@linkplain Course}
	 *         with the given id
	 */
	public Iterable<Course> findAll() {
		return coursesRepo.findAll();
	}

























    //Test Methods
    //TODO: Remove Before Production







    //Test Method, remove in final build
    public void TestCreateCourse() throws Exception {
        var name = "Rechnernetze";

        String[] lectureTitles = {"Einführung" , "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2", "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance", "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"};
        var lecture = new CourseInstance(CourseType.LECTURE, 1, 12, lectureTitles);

        courseInstancesRepo.save(lecture);

        String[] tutorialTitles = {"Einführung" , "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2", "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance", "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"};
        var tutorial = new CourseInstance(CourseType.TUTORIAL, 8, 12, tutorialTitles);

        courseInstancesRepo.save(tutorial);

        String[] seminarTitles = {"Einführung" , "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2", "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance", "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"};
        var seminar = new CourseInstance(CourseType.SEMINAR, 8, 12, seminarTitles);

        courseInstancesRepo.save(seminar);

        String[] pTitles = {"Einführung" , "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2", "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance", "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"};
        var practical = new CourseInstance(CourseType.TUTORIAL, 8, 12, pTitles);

        courseInstancesRepo.save(practical);

        var semesterOfStudents = 4;
        var faculty = CourseFaculty.COMPUTER_SCIENCE;
        var courseDate = LocalDate.of(2020, 10, 4);
        var semesterString = "SoSe 2020";

        coursesRepo.save(new Course(name, lecture, tutorial, seminar, practical, semesterOfStudents, faculty, semesterString, courseDate));
    }


}
