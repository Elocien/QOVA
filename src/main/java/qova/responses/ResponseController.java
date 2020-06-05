package qova.responses;

import java.io.IOException;
import java.util.Objects;
import java.util.Optional;

import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import qova.course.Course;
import qova.course.CourseRepository;
import qova.course.SurveyForm;

@Controller // This means that this class is a Controller
public class ResponseController {

    @Autowired
    private final ResponseManagement responseManagement;

    @Autowired
    private final CourseRepository courseRepository;

    @Autowired
    ResponseController(ResponseManagement responseManagement, CourseRepository courseRepository) {
        this.responseManagement = Objects.requireNonNull(responseManagement);
        this.courseRepository = Objects.requireNonNull(courseRepository);
    }

    // PostMapping to submit survey and serialize results
    // ---------------------------------------------------------------------------
    @PostMapping("/survey")
    public ResponseEntity recieveResponseJSON(SurveyForm form, @RequestParam String type,
            @RequestParam(required = false) String id) {

        // get JSON Response as string
        String JsonResponse = form.getQuestionnairejson();

        // Deserialize the String to a JavaObject Response (package qova.responses)
        // response = Deserialize(JsonResponse);

        // Save object
        // responseRepository.save(response)

        // if all goes well
        return ResponseEntity.ok(HttpStatus.OK);
    }

    // ---------------------------------------------------------------------------

    























    //test method
    @GetMapping("/createR")
    public String creatR() throws Exception {
        Optional<Course> crs = courseRepository.findById("c000000000000001");
        responseManagement.TestCreateResponses(crs.get());
        return "home";
    }

    

    @GetMapping("/genpdf")
    public HttpEntity<byte[]> generatePdf(HttpServletResponse response) throws Exception {
    
        //generate filename
        String filename = "testPdf";

        //Generate QRCode
        byte[] pdf = responseManagement.generatePDF();

        //Set HTTP headers and return HttpEntity
        HttpHeaders header = new HttpHeaders();
        header.setContentType(MediaType.APPLICATION_PDF);
        header.set(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + filename);
        header.setContentLength(pdf.length);

        return new HttpEntity<byte[]>(pdf, header);
    }
    
}
