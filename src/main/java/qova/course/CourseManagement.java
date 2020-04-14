package qova.course;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;


import com.google.zxing.BarcodeFormat;
import com.google.zxing.client.j2se.MatrixToImageWriter;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.qrcode.QRCodeWriter;

import javax.transaction.Transactional;

import java.awt.image.BufferedImage;

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

    // public void updateCourseSurvey(Long id, SurveyForm form){
    //     Optional<Course> crs = courses.findById(id);
    //     if (crs.isPresent()){
    //         Course course = crs.get();
            
    //     }
    // }
    
    
    public static BufferedImage generateQRCode(Course course) throws Exception {
    	String barcodeText = Long.toString(course.getId());
    	
   	 	QRCodeWriter barcodeWriter = new QRCodeWriter();
        BitMatrix bitMatrix = barcodeWriter.encode(barcodeText, BarcodeFormat.QR_CODE, 200, 200);
    
        return MatrixToImageWriter.toBufferedImage(bitMatrix);
    }
}
