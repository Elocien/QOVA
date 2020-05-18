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
import java.util.Objects;
import java.util.Optional;

@Service
@Transactional
public class CourseManagement {

    private final CourseRepository courses;

    @Autowired
    public CourseManagement(CourseRepository courses) {
        this.courses = Objects.requireNonNull(courses);
    }


    //Create Course and get Id from new course
    public String createCourseReturnId(CourseForm form) {
        Objects.requireNonNull(form);

        var name = form.getName();
        var lectureExists = form.getLectureExists();
        var tutorialExists = form.getTutorialExists();
        var seminarExists = form.getSeminarExists();  
        var classTotalSeminar = form.getClassTotalSeminar();
        var classTotalTutorial = form.getClassTotalTutorial();
        var semesterOfStudents = form.getSemesterOfStudents();
        var faculty = form.getFaculty();
        var courseInstance = parseSemesterString(form.getCourseInstance());

        Course crs  = new Course(name, lectureExists, tutorialExists, seminarExists, "", "", "", classTotalTutorial, classTotalSeminar, semesterOfStudents, faculty, courseInstance);
        courses.save(crs);
        
        return crs.getId();
    }

    
    //delete course
    public void deleteCourse(String id) {
        courses.deleteById(id);
    }



    //update course details
    public void updateCourseDetails(String id, CourseForm form){
        Optional<Course> crs = courses.findById(id);
        if (crs.isPresent()){
            Course course = crs.get();


            course.setName(form.getName());
            course.setLectureExists(form.getLectureExists());
            course.setTutorialExists(form.getTutorialExists());
            course.setSeminarExists(form.getSeminarExists());
            course.setClassTotalTutorial(form.getClassTotalTutorial());
            course.setClassTotalSeminar(form.getClassTotalSeminar());
            course.setSemesterOfStudents(form.getSemesterOfStudents());
            course.setFaculty(form.getFaculty());
            course.setCourseInstance(parseSemesterString(form.getCourseInstance()));
        }
    }



    //Gets the relevant Survey in the course objects, based on the given surveyType
    public String getSurveyforType (String id, String type){
        Optional<Course> crs = courses.findById(id);
        if (crs.isPresent()){
            Course course = crs.get();
            if (type.equals("LECTURE")){
                return course.getLectureSurvey();
            }
            else if (type.equals("SEMINAR")){
                return course.getSeminarSurvey();
            }
            else if (type.equals("TUTORIAL")){
                return course.getTutorialSurvey();
            }
        }
        return "Something went wrong";
    }



    //Sets the relevant Survey in the course objects, based on the given surveyType
    public void setSurveyforType (String id, String type, SurveyForm form){
        Optional<Course> crs = courses.findById(id);
        if (crs.isPresent()){
            
            Course course = crs.get();

            //if CourseType is Lecture, then save Survey as lectureSurvey
            if(type.equals("LECTURE")) {
                course.setLectureSurvey(form.getQuestionnairejson());
            }
            else if(type.equals("TUTORIAL")) {
                course.setTutorialSurvey(form.getQuestionnairejson());
            }
            else if (type.equals("SEMINAR")){
                course.setSeminarSurvey(form.getQuestionnairejson());
            }
        }
    }




    //QRCode Generator
    /**
     * 
     * @param text Takes a string as input (in our case a url)
     * @return A byte[] with the image of the QRCode
     * @throws WriterException
     * @throws IOException
     */
    public byte[] generateQRCodeImage(String text) throws WriterException, IOException {
        QRCodeWriter qrCodeWriter = new QRCodeWriter();
        BitMatrix bitMatrix = qrCodeWriter.encode(text, BarcodeFormat.QR_CODE, 300, 300);

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

        //Future Semesters to add (Given in years)
        int x = 1;

        //Previous Semesters to add (Given in years)
        int y = 1;



        //List sent to controller
        ArrayList<String> semesters = new ArrayList<String>();


        //Summer semester start is April(4), winter semester starts in October(10)
        //Winter semester spans over the new year, so it is of the format WiSe xx/yy


        //if winter semester and in year xx
        if(currentMonth <4){
            //previous semesters
            for(int i=x; i > 0; i--){
                semesters.add("WiSe " + String.valueOf(currentYear-(2+i)) + "/" + String.valueOf(currentYear-(1+i)));
                semesters.add("SoSe " + String.valueOf(currentYear-(1+i)));
            }

            //current Semester
            semesters.add("WiSe " + String.valueOf(currentYear-1) + "/" + String.valueOf(currentYear));

            //future semesters
            for(int i = 0; i < y; i++){
                semesters.add("SoSe " + String.valueOf(currentYear+i));
                semesters.add("WiSe " + String.valueOf(currentYear+i) + "/" + String.valueOf(currentYear + (i + 1)));
            }
        }
        
        //if winter semster and in year yy
        else if(currentMonth >= 10){
            //previous semesters
            for(int i=x; i > 0; i--){
                semesters.add("WiSe " + String.valueOf(currentYear-(1+i)) + "/" + String.valueOf(currentYear-(i)));
                semesters.add("SoSe " + String.valueOf(currentYear-i));
            }

            //current Semester
            semesters.add("WiSe " + String.valueOf(currentYear) + "/" + String.valueOf(currentYear + 1));

            //future semesters
            for(int i=0; i < y; i++){
                semesters.add("SoSe " + String.valueOf(currentYear+(1+i)));
                semesters.add("WiSe " + String.valueOf(currentYear+(1+i)) + "/" + String.valueOf(currentYear+(2+i)));
            }
        }


        //if summer semester
        else{
            //previous semesters
            for(int i=x; i > 0; i--){
                semesters.add("SoSe " + String.valueOf(currentYear-(1+i)));
                semesters.add("WiSe " + String.valueOf(currentYear-(1+i)) + "/" + String.valueOf(currentYear- i));
            }

            //current Semester
            semesters.add("SoSe " + String.valueOf(currentYear));

            //future semesters
            for(int i=0; i < y; i++){
                semesters.add("WiSe " + String.valueOf(currentYear+i) + "/" + String.valueOf(currentYear + (i + 2)));
                semesters.add("SoSe " + String.valueOf(currentYear+(1+i)));
                
            }
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































    //Test Method, remove in final build
    public void TestCreateCourse(){
        var name = "test";
        var lectureExists = true;
        var tutorialExists = true;
        var seminarExists = true;
        var classTotalTutorial = 10;
        var classTotalSeminar = 5;
        var semester = 3;
        var faculty = CourseFaculty.CHEMISTRY;

        courses.save(new Course(name, lectureExists, tutorialExists, seminarExists, "", "", "", classTotalTutorial, classTotalSeminar, semester, faculty, LocalDate.now()));
    }

   


}
