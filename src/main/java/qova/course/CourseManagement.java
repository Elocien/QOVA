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
        var semester = form.getSemester();
        var faculty = form.getFaculty();



        Course crs  = new Course(name, lectureExists, tutorialExists, seminarExists, "", "", "", classTotalTutorial, classTotalSeminar, semester, faculty);
        courses.save(crs);
        
        return crs.getId();
    }


    public void deleteCourse(String id) {
        courses.deleteById(id);
    }

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
            course.setSemester(form.getSemester());
            course.setFaculty(form.getFaculty());

        }
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
    public byte[] generateQRCodeImage(String text) throws WriterException, IOException {
        QRCodeWriter qrCodeWriter = new QRCodeWriter();
        BitMatrix bitMatrix = qrCodeWriter.encode(text, BarcodeFormat.QR_CODE, 300, 300);

        ByteArrayOutputStream pngOutputStream = new ByteArrayOutputStream();
        MatrixToImageWriter.writeToStream(bitMatrix, "PNG", pngOutputStream);
        byte[] pngData = pngOutputStream.toByteArray(); 
        return pngData;
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

        courses.save(new Course(name, lectureExists, tutorialExists, seminarExists, "some test string", "test string 2", "test string 3", classTotalTutorial, classTotalSeminar, semester, faculty));
    }



}
