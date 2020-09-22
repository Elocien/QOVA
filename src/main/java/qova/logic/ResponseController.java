package qova.logic;

import java.io.IOException;
import java.util.Objects;
import java.util.Optional;

import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

import qova.admin.AdminManagement;
import qova.enums.CourseType;
import qova.forms.SurveyForm;
import qova.objects.Course;
import qova.objects.SurveyResponse;


@Controller // This means that this class is a Controller
public class ResponseController {

    @Autowired
    private final ResponseManagement responseManagement;

    @Autowired
    private final CourseManagement courseManagement;

    @Autowired
    private final AdminManagement adminManagement;

    @Autowired
    ResponseController(ResponseManagement responseManagement, CourseManagement courseManagement, AdminManagement adminManagement) {
        this.responseManagement = Objects.requireNonNull(responseManagement);
        this.courseManagement = Objects.requireNonNull(courseManagement);
        this.adminManagement = Objects.requireNonNull(adminManagement);
    }

    //Error codes
    int courseNotFound = 1;
    int internalError = 2;


    //Mapping to which one is redirected to by the QRCode. This is where students enter which group and which topic they are handing their response in for
    //---------------------------------------------------------------------------

    @GetMapping("suveySelect")
    public String selectSurvey(Model model, @RequestParam String id, @RequestParam String type) {

        // course name, course type, instance names, groupAmount
        Optional<Course> crs = courseManagement.findById(id);
        if (crs.isPresent()) {
            model.addAttribute("courseName", crs.get().getName());
            model.addAttribute("courseType", type);

            if (type.equals("LECTURE")) {
                model.addAttribute("instanceTitles", crs.get().getLecture().getInstanceTitles());
                model.addAttribute("groupAmount", crs.get().getLecture().getGroupAmount());
            }
            if (type.equals("TUTORIAL")) {
                model.addAttribute("instanceTitles", crs.get().getLecture().getInstanceTitles());
                model.addAttribute("groupAmount", crs.get().getLecture().getGroupAmount());
            }
            if (type.equals("SEMINAR")) {
                model.addAttribute("instanceTitles", crs.get().getLecture().getInstanceTitles());
                model.addAttribute("groupAmount", crs.get().getLecture().getGroupAmount());
            }
            if (type.equals("PRACTICAL")) {
                model.addAttribute("instanceTitles", crs.get().getLecture().getInstanceTitles());
                model.addAttribute("groupAmount", crs.get().getLecture().getGroupAmount());
            }
            return "surveySelect";
        }
        
        //if course does not exist, redirect to global error page
        return "error?code=" + courseNotFound;
    }

    // ---------------------------------------------------------------------------

    // TODO: rename path variables
    // Validation of entry of surveySelect page, and redirect to the actual survey
    @PostMapping("surveySelect")
    public String selectSurveySubmission(Model model, @RequestParam String id, @RequestParam String type,
            @RequestParam String instanceTitle, @RequestParam Integer groupAmount) {

        Optional<Course> crs = courseManagement.findById(id);

        //if anything is null or not an allowed value, redirect back
        if(!crs.isPresent()){
            return "error?code=" + courseNotFound;
        }
        // if type is not one of the defined values

        if(!(type.equals("LECTURE")) && !(type.equals("TUTORIAL")) && !(type.equals("SEMINAR")) && !(type.equals("PRACTICAL"))){
            return "error?code=" + internalError;
        }


        //TODO validate that parameters only contain valid charachters. E.g. a-zA-Z0-9

        else {
            return "survey?type=" + type + "&id=" + id + "instanceTitle=" + instanceTitle + "groupNumber="
                    + groupAmount;
        }
    }

    // Get Survey from Server
    // ---------------------------------------------------------------------------

    // Mapping for Survey HTML
    @GetMapping("survey")
    public String SurveyView(Model model, @RequestParam String type, @RequestParam(required = false) String id) {
        // redirect
        if (id == null) {
			return "error?code=" + courseNotFound;
        }

        // fetch course and go to details if present
        Optional<Course> course = courseManagement.findById(id);

        // Validate that course exists, and that the survey is not empty
        if (course.isPresent()) {
            String survey = courseManagement.getSurveyforType(id, type);
            if (survey.equals("Something went wrong")) {
                return "redirect:/";
            } else {

                //Concatenate the default survey to the course survey
                survey = adminManagement.concatenateDefaultSurveyToSurveyString(survey, responseManagement.parseCourseType(type));

                model.addAttribute("typeID", type);
                model.addAttribute("id", id);
                model.addAttribute("survey", survey);
                model.addAttribute("coursename", course.get().getName());
                return "survey";
            }

        }
        
        //If condition not met, redirect to home
        else{
            return "error?code=" + courseNotFound;
        }
    }

    // PostMapping to submit survey and serialize results
    // ---------------------------------------------------------------------------
    @PostMapping("/survey")
    public String recieveResponseJSON(SurveyForm form, @RequestParam String type, @RequestParam String id) {

        // get JSON Response as string
        String JsonResponse = form.getQuestionnairejson();

        // Deserialize the String to a JavaObject Response (package qova.responses)
        // response = Deserialize(JsonResponse);

        // Save object
        // responseRepository.save(response)

        // if all goes well
        return "postSubmissionLanding";
    }

    // ---------------------------------------------------------------------------

    /**
     * Used to retrieve the results for a given questionnaire
     * 
     * @param model {@link org.springframework.ui.Model}
     * @param type {@linkplain qova.enums.CourseType}
     * @param id The Id of the {@linkplain qova.objects.Course}
     * @param group The groupNumber of the {@linkplain qova.objects.Course}
     * @param instance The instanceNumber of the {@linkplain qova.objects.Course}
     * @return The surveyResults template, which shows the compiled results of the requested questionnaire
     */
    @GetMapping("/surveyresults")
    public String surveyResultsTest(Model model, @RequestParam String type, @RequestParam String id, @RequestParam String group, @RequestParam String instance) {
        Optional<Course> crs = courseManagement.findById(id);
        if(crs.isPresent()){
            Optional<SurveyResponse> srv = responseManagement.findByCourseAndCourseTypeAndClassNo(crs.get(), responseManagement.parseCourseType(type), Integer.valueOf(group), Integer.valueOf(instance));
            if(srv.isPresent()){
                model.addAttribute("response", srv.get());
            }
        }
        return "surveyResults";
    }




    // PDF Generation
    @GetMapping("/generatePDF")
    public HttpEntity<byte[]> generatePdf(@RequestParam String id, @RequestParam String type, @RequestParam String groupNumber, @RequestParam String instanceNumber, HttpServletResponse response)
            throws NumberFormatException, IOException, Exception {

        // generate filename
        String filename = "testPdf.pdf";

        Optional<Course> crs = courseManagement.findById(id);

        // verify that course is present
        if (!crs.isPresent()) {
            return null;
        }

        // Try to parse the courseType
        CourseType courseType = responseManagement.parseCourseType(type);
        if (courseType == null) {
            return null;
        }

        // Generate PDF
        byte[] pdf = responseManagement.generatePDF_en(crs.get(), courseType, Integer.parseInt(groupNumber),
                Integer.parseInt(instanceNumber));

        // Set HTTP headers and return HttpEntity
        HttpHeaders header = new HttpHeaders();
        header.setContentType(MediaType.APPLICATION_PDF);
        header.set(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + filename);
        header.setContentLength(pdf.length);

        return new HttpEntity<byte[]>(pdf, header);
    }

   
    // CSV Generation
    @GetMapping("/generateCSV")
    public HttpEntity<byte[]> generateCsv(@RequestParam String id, @RequestParam String type, @RequestParam String groupNumber, @RequestParam String instanceNumber, HttpServletResponse response)
            throws Exception {
    
        //generate filename
        String filename = "testCsv.csv";

        Optional<Course> crs = courseManagement.findById(id);

        //verify that course is present
        if(!crs.isPresent()){
            return null;
        }

        //Try to parse the courseType
        CourseType courseType = responseManagement.parseCourseType(type);
        if(courseType == null){
            return null;
        }

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
    public String createR() throws Exception {
        Optional<Course> crs = courseManagement.findById("c000000000000001");
        responseManagement.createTestResponses(crs.get());
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

    @GetMapping("/surveyResults")
    public String surveyResults(Model model) throws Exception {

        Course course = courseManagement.TimTestCreateCourse();
        SurveyResponse rsp = responseManagement.timCreateTestResponses(course);
        
        model.addAttribute("response", rsp);

        return "surveyResults";
    }

    
}
    

    
