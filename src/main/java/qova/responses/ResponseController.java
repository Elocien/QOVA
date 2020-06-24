package qova.responses;

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
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

import qova.course.Course;
import qova.course.CourseManagement;
import qova.course.CourseType;
import qova.course.SurveyForm;

@Controller // This means that this class is a Controller
public class ResponseController {

    @Autowired
    private final ResponseManagement responseManagement;

    @Autowired
    private final CourseManagement courseManagement;

    @Autowired
    ResponseController(ResponseManagement responseManagement, CourseManagement courseManagement) {
        this.responseManagement = Objects.requireNonNull(responseManagement);
        this.courseManagement = Objects.requireNonNull(courseManagement);
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



    //PDF Generation
    @GetMapping("/generatePDF")
    public HttpEntity<byte[]> generatePdf(@RequestParam String id, @RequestParam String type, @RequestParam String classNo, HttpServletResponse response) throws Exception {
    
        //generate filename
        String filename = "testPdf.pdf";

        //Get the course;
        Optional<Course> crs = courseManagement.findById(id);

        //verify that course is present
        if(!crs.isPresent()){
            throw new Exception("No course found");
        }

        //Try to parse the courseType
        CourseType courseType;
        if (type.equals("LECTURE")) {courseType = CourseType.LECTURE;}
        else if (type.equals("TUTORIAL")) {courseType = CourseType.TUTORIAL;}
        else if (type.equals("SEMINAR")) {courseType = CourseType.SEMINAR;}
        else {throw new Exception("No courseType given");}

        //Generate PDF
        byte[] pdf = responseManagement.generatePDF_en(crs.get(), courseType, Integer.parseInt(classNo));

        //Set HTTP headers and return HttpEntity
        HttpHeaders header = new HttpHeaders();
        header.setContentType(MediaType.APPLICATION_PDF);
        header.set(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + filename);
        header.setContentLength(pdf.length);

        return new HttpEntity<byte[]>(pdf, header);
    }
    

    //CSV Generation
    @GetMapping("/generateCSV")
    public HttpEntity<byte[]> generateCsv(@RequestParam String id, @RequestParam String type, @RequestParam String classNo, HttpServletResponse response) throws Exception {
    
        //generate filename
        String filename = "testCsv.csv";

        //Get the course;
        Optional<Course> crs = courseManagement.findById(id);

        //verify that course is present
        if(!crs.isPresent()){
            throw new Exception("No course found");
        }

        //Try to parse the courseType
        CourseType courseType;
        if (type.equals("LECTURE")) {courseType = CourseType.LECTURE;}
        else if (type.equals("TUTORIAL")) {courseType = CourseType.TUTORIAL;}
        else if (type.equals("SEMINAR")) {courseType = CourseType.SEMINAR;}
        else {throw new Exception("No courseType given");}

        //Generate PDF
        byte[] pdf = responseManagement.generateCSV_en(crs.get(), courseType, Integer.parseInt(classNo));

        //Set HTTP headers and return HttpEntity
        HttpHeaders header = new HttpHeaders();
        header.setContentType(MediaType.APPLICATION_PDF);
        header.set(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + filename);
        header.setContentLength(pdf.length);

        return new HttpEntity<byte[]>(pdf, header);
    }







    





























    //test method
    @GetMapping("/createR")
    public String creatR() throws Exception {
        Optional<Course> crs = courseManagement.findById("c000000000000001");
        responseManagement.TestCreateResponses(crs.get());
        return "home";
    }

    //PDF Generation
    @GetMapping("/pdftest")
    public HttpEntity<byte[]> pdfTest(HttpServletResponse response) throws Exception {
    
        //generate filename
        String filename = "testPdf.pdf";

        //Generate PDF
        byte[] pdf = responseManagement.generatePDF_test();

        //Set HTTP headers and return HttpEntity
        HttpHeaders header = new HttpHeaders();
        header.setContentType(MediaType.APPLICATION_PDF);
        header.set(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + filename);
        header.setContentLength(pdf.length);

        return new HttpEntity<byte[]>(pdf, header);
    }


    @GetMapping("json")
    public void testJSONParsing(){

        //How to use:
        //Set a survey using questioneditor, for the LECTURE survey with id = c000000000000001 (this is always the first survey generated). Then find the string in the
        //database to figure out its strucutre. Then try to parse it (using the method below), and see if you can retrieve the intended values.

        //Test String
        String json = courseManagement.findById("c000000000000001").get().getLecture().getSurvey();

        // https://stackoverflow.com/questions/2591098/how-to-parse-json-in-java

    }

}
    

    
