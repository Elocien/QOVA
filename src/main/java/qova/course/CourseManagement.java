package qova.course;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.zxing.BarcodeFormat;
import com.google.zxing.WriterException;
import com.google.zxing.client.j2se.MatrixToImageWriter;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.qrcode.QRCodeWriter;

import javax.transaction.Transactional;

import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.util.Objects;
import java.util.Optional;

@Service
@Transactional
public class CourseManagement {

    private final CourseRepository courses;

    // test attributes
    private Long id = 1L;

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


    public void deleteCourse(Long id) {
        courses.deleteById(id);
    }

    public void updateCourseDetails(Long id, CourseForm form){
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

    public void updateCourseSurvey(Long id, CourseForm form){
        Optional<Course> crs = courses.findById(id);
        if (crs.isPresent()){
            Course course = crs.get();
            course.setSurvey(form.getSurvey());
        }
    }
    
    
    //baeldung version 

    // public static BufferedImage generateQRCode(String barcodeText) throws Exception {
    //     QRCodeWriter barcodeWriter = new QRCodeWriter();
    //     BitMatrix bitMatrix = barcodeWriter.encode(barcodeText, BarcodeFormat.QR_CODE, 200, 200);
     
    //     return MatrixToImageWriter.toBufferedImage(bitMatrix);
    // }


    //callicoder version

    public byte[] generateQRCodeImage(String text) throws WriterException, IOException {
        QRCodeWriter qrCodeWriter = new QRCodeWriter();
        BitMatrix bitMatrix = qrCodeWriter.encode(text, BarcodeFormat.QR_CODE, 300, 300);

        ByteArrayOutputStream pngOutputStream = new ByteArrayOutputStream();
        MatrixToImageWriter.writeToStream(bitMatrix, "PNG", pngOutputStream);
        byte[] pngData = pngOutputStream.toByteArray(); 
        return pngData;
    }
}
