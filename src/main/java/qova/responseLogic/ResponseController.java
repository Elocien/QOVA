package qova.responseLogic;

import java.util.Arrays;
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
import org.springframework.web.bind.annotation.ResponseBody;

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


    //Mapping to which one is redirected to by the QRCode. This is where students enter which group and which topic they are handing their response in for
    //---------------------------------------------------------------------------

    @GetMapping("suveySelect")
    public String selectSurvey(Model model, @RequestParam String id, @RequestParam String type){
        
        //course name, course type, instance names, groupAmount
        Optional<Course> crs = courseManagement.findById(id);
        if(crs.isPresent()){
            model.addAttribute("courseName", crs.get().getName());
            model.addAttribute("courseType", type);

            if(type.equals("LECTURE")){
                model.addAttribute("instanceTitles", crs.get().getLecture().getInstanceTitles());
                model.addAttribute("groupAmount", crs.get().getLecture().getGroupAmount());
            }
            if(type.equals("TUTORIAL")){
                model.addAttribute("instanceTitles", crs.get().getLecture().getInstanceTitles());
                model.addAttribute("groupAmount", crs.get().getLecture().getGroupAmount());
            }
            if(type.equals("SEMINAR")){
                model.addAttribute("instanceTitles", crs.get().getLecture().getInstanceTitles());
                model.addAttribute("groupAmount", crs.get().getLecture().getGroupAmount());
            }
            if(type.equals("PRACTICAL")){
                model.addAttribute("instanceTitles", crs.get().getLecture().getInstanceTitles());
                model.addAttribute("groupAmount", crs.get().getLecture().getGroupAmount());
            }
            return "surveySelect";
        }
        
        //if course does not exist, redirect to global error page
        return "error";
    }

    //---------------------------------------------------------------------------





    //TODO: rename path variables
    //Validation of entry of surveySelect page, and redirect to the actual survey
    @PostMapping("surveySelect")
    public String selectSurveySubmission(Model model, @RequestParam String id, @RequestParam String type, @RequestParam String instanceTitle, @RequestParam Integer groupAmount){
        
        Optional<Course> crs = courseManagement.findById(id);

        //if anything is null or not an allowed value, redirect back
        if(!crs.isPresent()){
            //TODO: set error code "Course not present. You are unable to submit a response to this survey"
            return "error";
        }
        //if type is not one of the defined values

        //for easy access
        Course course = crs.get();

        if(!(type.equals("LECTURE")) && !(type.equals("TUTORIAL")) && !(type.equals("SEMINAR")) && !(type.equals("PRACTICAL"))){
            //TODO: redirect to error page with code 02
            return "error";
        }


        //if instanceTitle is not an element of the InstanceTitles
        else if (type.equals("LECTURE") && !(Arrays.stream(course.getLecture().getInstanceTitles()).anyMatch(instanceTitle::equals))){
            return "error";   
        }
        //if groupAmount exceedes the number set, or is less than 0, redirect to error page
        else if(type.equals("LECTURE") && groupAmount > course.getLecture().getGroupAmount() || groupAmount < 0){
            return "error";
        }


        //if instanceTitle is not an element of the InstanceTitles
        else if (type.equals("TUTORIAL") && !(Arrays.stream(course.getTutorial().getInstanceTitles()).anyMatch(instanceTitle::equals))){
            return "error";   
        }
        //if groupAmount exceedes the number set, or is less than 0, redirect to error page
        else if(type.equals("TUTORIAL") && groupAmount > course.getTutorial().getGroupAmount() || groupAmount < 0){
            return "error";
        }


        //if instanceTitle is not an element of the InstanceTitles
        else if (type.equals("SEMINAR") && !(Arrays.stream(course.getSeminar().getInstanceTitles()).anyMatch(instanceTitle::equals))){
            return "error";   
        }
        //if groupAmount exceedes the number set, or is less than 0, redirect to error page
        else if(type.equals("SEMINAR") && groupAmount > course.getSeminar().getGroupAmount() || groupAmount < 0){
            return "error";
        }


        //if instanceTitle is not an element of the InstanceTitles
        else if (type.equals("PRACTICAL") && !(Arrays.stream(course.getPractical().getInstanceTitles()).anyMatch(instanceTitle::equals))){
            return "error";   
        }
        //if groupAmount exceedes the number set, or is less than 0, redirect to error page
        else if(type.equals("PRACTICAL") && groupAmount > course.getPractical().getGroupAmount() || groupAmount < 0){
            return "error";
        }


        else{return "survey?type=" + type + "&id=" + id + "instanceTitle=" + instanceTitle + "groupNumber=" + groupAmount;}
    }






    //Get Survey from Server 
    //---------------------------------------------------------------------------

    //Mapping for Survey HTML
    @GetMapping("survey")
    public String SuveyView (Model model, @RequestParam String type, @RequestParam(required = false) String id){
        //redirect 
        if (id == null) {
			return "redirect:/";
        }
        
        //fetch course and go to details if present
        Optional<Course> course = courseManagement.findById(id);

        //Validate that course exists, and that the survey is not empty
        if (course.isPresent()){
            String survey = courseManagement.getSurveyforType(id,type);
            if (survey.equals("Something went wrong")){
                return "redirect:/";
            }
            else {
                model.addAttribute("typeID", type);
                model.addAttribute("id", id);
                model.addAttribute("survey",survey);
                model.addAttribute("coursename", course.get().getName());
                return "survey";
            }

        }
        
        //If condition not met, redirect to home
        else{
            return "redirect:/";
        }
    }
    //----------------------------------------------------------------------------


    //Mapping to recieve SURVEY (Formatted as JSON) from server
    @GetMapping("survey/get")
    @ResponseBody
    public String sendSurvey( @RequestParam String type, @RequestParam(required = false) String id){

        //redirect 
        if (id == null) {
			return null;
        }

        else{
            //Retrieve survey
            String JsonString = courseManagement.getSurveyforType(id, type);

            //return the JSON
            return JsonString;
        }
    }



    // PostMapping to submit survey and serialize results
    // ---------------------------------------------------------------------------
    @PostMapping("/survey")
    public ResponseEntity recieveResponseJSON(SurveyForm form, @RequestParam String type,
            @RequestParam String id) {

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
    public HttpEntity<byte[]> generatePdf(@RequestParam String id, @RequestParam String type, @RequestParam String groupNumber, @RequestParam String instanceNumber, HttpServletResponse response) throws Exception {
    
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
        byte[] pdf = responseManagement.generatePDF_en(crs.get(), courseType, Integer.parseInt(groupNumber), Integer.parseInt(instanceNumber));

        //Set HTTP headers and return HttpEntity
        HttpHeaders header = new HttpHeaders();
        header.setContentType(MediaType.APPLICATION_PDF);
        header.set(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + filename);
        header.setContentLength(pdf.length);

        return new HttpEntity<byte[]>(pdf, header);
    }
    

    //CSV Generation
    @GetMapping("/generateCSV")
    public HttpEntity<byte[]> generateCsv(@RequestParam String id, @RequestParam String type, @RequestParam String groupNumber, @RequestParam String instanceNumber, HttpServletResponse response) throws Exception {
    
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
        byte[] pdf = responseManagement.generateCSV_en(crs.get(), courseType, Integer.parseInt(groupNumber), Integer.parseInt(instanceNumber));

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

}
    

    
