package qova.responses;

import java.io.IOException;
import java.util.Objects;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

import qova.course.Course;
import qova.course.CourseRepository;
import qova.course.SurveyForm;

@Controller // This means that this class is a Controller
public class ResponseController {

    @Autowired
    private final ResponseManagement responseManagement;

    @Autowired
    private final ResponseRepository responseRepository;

    @Autowired
    private final CourseRepository courseRepository;

    @Autowired
    ResponseController(ResponseManagement responseManagement, ResponseRepository responseRepository,
            CourseRepository courseRepository) {
        this.responseManagement = Objects.requireNonNull(responseManagement);
        this.responseRepository = Objects.requireNonNull(responseRepository);
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

    // // test method
    // @GetMapping("/barchart")
    // public String barchartTest() throws IOException {
    //     responseManagement.GeneratePDF();
    //     return "home";
    // }



    //test method
    @GetMapping("/createR")
    public String creatR(){
        Optional<Course> crs = courseRepository.findById("c000000000000001");
        responseManagement.TestCreateResponses(crs.get());
        return "home";
    }
}
