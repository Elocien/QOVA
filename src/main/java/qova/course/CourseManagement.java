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
        var courseInstance = form.getCourseInstance();

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

        }
    }

    //getter for the Survey String
    public String getSurveyforTyp ( String id, CourseType type){
        Optional<Course> crs = courses.findById(id);
        if (crs.isPresent()){
            Course course = crs.get();
            if (type == CourseType.LECTURE){
                return course.getLectureSurvey();
            }
            else if (type == CourseType.SEMINAR){
                return course.getSeminarSurvey();
            }
            else if (type == CourseType.TUTORIAL){
                return course.getTutorialSurvey();
            }
        }
        return "Something went wrong";
    }



    //update the lecture survey
    public void updateLectureSurvey(String id, CourseForm form){
        Optional<Course> crs = courses.findById(id);
        if (crs.isPresent()){
            Course course = crs.get();
            course.setLectureSurvey(form.getLectureSurvey());
        }
    }



    //update the tutorial survey
    public void updateTutorialSurvey(String id, CourseForm form){
        Optional<Course> crs = courses.findById(id);
        if (crs.isPresent()){
            Course course = crs.get();
            course.setTutorialSurvey(form.getTutorialSurvey());
        }
    }



    //update the seminar survey
    public void updateSeminarSurvey(String id, CourseForm form){
        Optional<Course> crs = courses.findById(id);
        if (crs.isPresent()){
            Course course = crs.get();
            course.setSeminarSurvey(form.getSeminarSurvey());
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
     * Function used to populate a drop down menu in course creation UI. Fills a list with (x) future semesters and (y) previous semsters, as well as the current semester
     * @return
     */
    public ArrayList<LocalDate> findSemesters(){
        
        //Current Date
        LocalDate now = LocalDate.now();

        //Current year and month as ints
        int currentYear = now.getYear();
        int currentMonth = now.getMonthValue();

        //Future Semesters to add (Given in years)
        int x = 1;

        //Previous Semesters to add (Given in years)
        int y = 1;



        //List sent to controller
        ArrayList<LocalDate> semesters = new ArrayList<LocalDate>();


        //Summer semester start is April(4), winter semester starts in October(10)
        //Winter semester spans over the new year, so it is of the format WiSe xx/yy


        //if winter semester and in year xx
        if(currentMonth <4){
            //previous semesters
            for(int i=0; i < x; i++){
                semesters.add(LocalDate.of((currentYear-(2+i)), 10, 1));
                semesters.add(LocalDate.of((currentYear-(1+i)), 4, 1));
            }

            //current Semester
            semesters.add(LocalDate.of((currentYear-1), 10, 1));

            //future semesters
            for(int i=0; i < y; i++){
                semesters.add(LocalDate.of((currentYear+i), 4, 1));
                semesters.add(LocalDate.of((currentYear+i), 10, 1));
            }
        }
        
        //if winter semster and in year yy
        else if(currentMonth >= 10){
            //previous semesters
            for(int i=0; i < x; i++){
                semesters.add(LocalDate.of((currentYear-(1+i)), 10, 1));
                semesters.add(LocalDate.of((currentYear-i), 4, 1));
            }

            //current Semester
            semesters.add(LocalDate.of((currentYear), 10, 1));

            //future semesters
            for(int i=0; i < y; i++){
                semesters.add(LocalDate.of((currentYear+(1+i)), 4, 1));
                semesters.add(LocalDate.of((currentYear+(1+i)), 10, 1));
            }
        }


        //if summer semester
        else{
            //previous semesters
            for(int i=0; i < x; i++){
                semesters.add(LocalDate.of((currentYear-(1+i)), 4, 1));
                semesters.add(LocalDate.of((currentYear-(1+i)), 10, 1));
            }

            //current Semester
            semesters.add(LocalDate.of((currentYear), 4, 1));

            //future semesters
            for(int i=0; i < y; i++){
                semesters.add(LocalDate.of((currentYear+i), 10, 1));
                semesters.add(LocalDate.of((currentYear+(1+i)), 4, 1));
            }
        }
        

        //return arraylist
        return semesters;
    }

    public Course saveCourse ( Course course){
        return courses.save(course);
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
