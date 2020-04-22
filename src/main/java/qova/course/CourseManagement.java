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

    public Course createCourse(CourseForm form) {
        Objects.requireNonNull(form);

        var name = form.getName();
        var type = form.getType();
        var survey = form.getSurvey();
        var classTotal = form.getClassTotal();
        var semester = form.getSemester();
        var faculty = form.getFaculty();

        return courses.save(new Course(name, type, survey, classTotal, semester, faculty));
    }


    public void deleteCourse(String id) {
        courses.deleteById(id);
    }

    public void updateCourseDetails(String id, CourseForm form){
        Optional<Course> crs = courses.findById(id);
        if (crs.isPresent()){
            Course course = crs.get();
            course.setName(form.getName());
            course.setType(form.getType());
            course.setClassTotal(form.getClassTotal());
            course.setSemester(form.getSemester());
            course.setFaculty(form.getFaculty());

        }
    }

    public void updateCourseSurvey(String id, CourseForm form){
        Optional<Course> crs = courses.findById(id);
        if (crs.isPresent()){
            Course course = crs.get();
            course.setSurvey(form.getSurvey());
        }
    }


    //callicoder version

    public byte[] generateQRCodeImage(String text) throws WriterException, IOException {
        QRCodeWriter qrCodeWriter = new QRCodeWriter();
        BitMatrix bitMatrix = qrCodeWriter.encode(text, BarcodeFormat.QR_CODE, 300, 300);

        ByteArrayOutputStream pngOutputStream = new ByteArrayOutputStream();
        MatrixToImageWriter.writeToStream(bitMatrix, "PNG", pngOutputStream);
        byte[] pngData = pngOutputStream.toByteArray(); 
        return pngData;
    }


    public void TestCreateCourse(){
        var name = "test";
        var type = CourseType.LECTURE;
        String[] survey = {"test 1, test 2"};
        var classTotal = 10;
        var semester = 3;
        var faculty = CourseFaculty.CHEMISTRY;

        courses.save(new Course(name, type, survey, classTotal, semester, faculty));
    }
}
