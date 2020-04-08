package qova.course;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;

// import com.google.zxing.BarcodeFormat;
// import com.google.zxing.client.j2se.MatrixToImageWriter;
// import com.google.zxing.common.BitMatrix;
// import com.google.zxing.qrcode.QRCodeWriter;

// import java.awt.image.BufferedImage;

import java.util.Objects;

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

    public void createCourse() {
        Course n = new Course();
        n.setName("testname");
        n.setType(CourseType.LECTURE);
        n.setClassTotal(10);
        n.setSemester(6);
        n.setFaculty(CourseFaculty.COMPUTER_SCIENCE);
        courses.save(n);
    }

    public void deleteSurvey() {
        courses.deleteById(id);
        id++;
    }

    // public static BufferedImage generateQRCodeImage(String barcodeText) throws Exception {
    //     QRCodeWriter barcodeWriter = new QRCodeWriter();
    //     BitMatrix bitMatrix = barcodeWriter.encode(barcodeText, BarcodeFormat.QR_CODE, 200, 200);
     
    //     return MatrixToImageWriter.toBufferedImage(bitMatrix);
    // }
}
