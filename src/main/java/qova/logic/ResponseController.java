package qova.logic;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

import qova.admin.AdminManagement;
import qova.enums.CourseType;
import qova.forms.SurveyForm;
import qova.forms.SurveySelectForm;
import qova.objects.AbstractResponse;
import qova.objects.Course;
import qova.objects.CourseInstance;
import qova.objects.SurveyResponse;

//Temporary Import
import java.util.ArrayList;

@Controller // This means that this class is a Controller
public class ResponseController {

    @Autowired
    private final ResponseManagement responseManagement;

    @Autowired
    private final CourseManagement courseManagement;

    @Autowired
    private final AdminManagement adminManagement;

    @Autowired
    ResponseController(ResponseManagement responseManagement, CourseManagement courseManagement,
            AdminManagement adminManagement) {
        this.responseManagement = Objects.requireNonNull(responseManagement);
        this.courseManagement = Objects.requireNonNull(courseManagement);
        this.adminManagement = Objects.requireNonNull(adminManagement);
    }

    // Error codes

    // Mapping to which one is redirected to by the QRCode. This is where students
    // enter which group and which topic they are handing their response in for
    // ---------------------------------------------------------------------------

    @GetMapping("surveySelect")
    @PreAuthorize("hasAnyRole('STAFF','STUDENT')")
    public String selectSurvey(Model model, SurveySelectForm form, @RequestParam String mode, @RequestParam UUID id,
            @RequestParam(required = false, defaultValue = "") String type) {

        // course name, course type, instance names, groupAmount
        Optional<Course> crs = courseManagement.findById(id);
        if (crs.isPresent()) {

            Course course = crs.get();

            model.addAttribute("course", course);
            model.addAttribute("courseName", course.getName());
            model.addAttribute("id", course.getId());
            model.addAttribute("form", form);
            model.addAttribute("mode", mode);

            //Get the CourseType
            CourseType courseType = responseManagement.parseCourseType(type);
            //
            if (courseType == null) {
                model.addAttribute("typeExists", false);
            } else {
                model.addAttribute("typeExists", true);
                model.addAttribute("type", type);
                model.addAttribute("instanceTitles", course.getInstance(courseType).getInstanceTitles());
                model.addAttribute("groupAmount", course.getInstance(courseType).getGroupAmount());
            }

            return "surveySelect";
        }

        // if course does not exist, redirect to global error page
        return "error";
    }

    // ---------------------------------------------------------------------------

    // Validation of entry of surveySelect page, and redirect to the actual survey
    @PostMapping("surveySelect")
    @PreAuthorize("hasAnyRole('STAFF','STUDENT')")
    public String selectSurveySubmission(Model model, @ModelAttribute("form") SurveySelectForm form,
            @RequestParam String mode, @RequestParam String type, @RequestParam UUID id, Authentication authentication) {

        Optional<Course> crs = courseManagement.findById(id);

        // if anything is null or not an allowed value, redirect back
        if (crs.isEmpty()) {
            return "error";
        }
        // if type is not one of the defined values
        if (responseManagement.parseCourseType(type) == null) {
            return "error";
        }

        // TODO validate that parameters only contain valid charachters. E.g. a-zA-Z0-9

        else {

            if (mode.equals("participant")) {
                return "redirect:/survey?id=" + id + "&type=" + type + "&instance=" + form.getInstance() + "&group="
                        + form.getGroup();
            } else if (mode.equals("results")) {
                return "redirect:/surveyResults?id=" + id + "&type=" + type + "&instance=" + form.getInstance() + "&group="
                        + form.getGroup();
            } else {
                return "error";
            }
        }
    }

    // Get Survey from Server
    // ---------------------------------------------------------------------------

    // Mapping for Survey HTML
    @GetMapping("survey")
    public String surveyView(Model model, @RequestParam(required = false) UUID id,
            @RequestParam(required = false) String type, @RequestParam(required = false) String group,
            @RequestParam(required = false) String instance, Authentication authentication) {


            // redirect
        if (id == null || type == null || group == null || instance == null) {
            return "error";
        }

        // fetch course and go to details if present
        Optional<Course> course = courseManagement.findById(id);

        // Validate that course exists, and that the survey is not empty
        if (course.isPresent()) {
            String survey = courseManagement.getSurveyforType(id, responseManagement.parseCourseType(type));
            if (survey.equals("Something went wrong")) {
                return "redirect:/";
            } else {

                // Concatenate the default survey to the course survey
                survey = adminManagement.concatenateDefaultSurveyToSurveyString(survey,
                        responseManagement.parseCourseType(type));

                model.addAttribute("group", group);
                model.addAttribute("instance", instance);
                model.addAttribute("typeID", type);
                model.addAttribute("id", id);
                model.addAttribute("survey", survey);
                model.addAttribute("coursename", course.get().getName());
                return "survey";
            }

        }

        // If condition not met, redirect to home
        else {
            return "error";
        }
    }

    // PostMapping to submit survey and serialize results
    // ---------------------------------------------------------------------------
    @PostMapping("/survey")
    public String recieveResponseJSON(Model model, SurveyForm form, @RequestParam(required = false) UUID id,
            @RequestParam(required = false) String type, @RequestParam(required = false) String group,
            @RequestParam(required = false) String instance, Authentication authentication) {

        if (id == null || type == null || group == null || instance == null) {
            return "error";
        }

        // Get the StudentId, through the authentication object
        String studentId = authentication.getPrincipal().toString();

        // Get the CourseType
        CourseType courseType = responseManagement.parseCourseType(type);

        // Initialise JSONArray
        JSONArray studentResponseJson;

        // get JSON Response
        try {
            studentResponseJson = new JSONArray(form.getQuestionnaireJson());
        } catch (Exception e) {
            return "error";
        }

        // fetch course
        Optional<Course> crs = courseManagement.findById(id);

        // Validate that course exists, and that the survey is not empty
        if (crs.isPresent()) {

            Course course = crs.get();

            // Verify that the sent JSON is not malicious
            responseManagement.verifyStudentResponseJson(studentResponseJson,
                    course.getInstance(courseType).getSurvey());

            Optional<SurveyResponse> survRsp = responseManagement
                    .findSurveyResponseByCourseAndCourseTypeAndGroupNumberAndInstanceNumber(course, courseType,
                            Integer.valueOf(group), Integer.valueOf(instance));

            if (survRsp.isPresent()) {
                SurveyResponse surveyResponse = survRsp.get();

                // Redirect the user to SurveyReject, because they have already completed a survey
                if (surveyResponse.getListOfStudentsThatSubmitted().contains(studentId)){
                    model.addAttribute("id", id);
                    model.addAttribute("courseType", type);
                    return "surveyReject";
                }

                // Increments Submission Counter
                surveyResponse.addStundentIdToSubmissionListAndIncrementCounter(studentId);

                // The manager method that increments & sets the correct values
                responseManagement.submitStudentResponse(surveyResponse, studentResponseJson);

            }

        }

        // Manager Method
        // Increment numberOfSubmissions in SurveyResponse
        // Add stundent ID to SurveyResponse List

        // if all goes well
        return "surveyCheckout";
    }

    // ---------------------------------------------------------------------------

    /**
     * Used to retrieve the results for a given questionnaire.
     * 
     * @param model    {@link org.springframework.ui.Model}
     * @param type     {@linkplain qova.enums.CourseType}
     * @param id       The Id of the {@linkplain qova.objects.Course}
     * @param group    The groupNumber of the {@linkplain qova.objects.Course}
     * @param instance The instanceNumber of the {@linkplain qova.objects.Course}
     * @return The surveyResults template, which shows the compiled results of the
     *         requested questionnaire
     */
    @GetMapping("/surveyResults")
    public String surveyResults(Model model, @RequestParam String type, @RequestParam UUID id,
            @RequestParam String group, @RequestParam String instance) {

        // The Course object in an optional
        Optional<Course> crs = courseManagement.findById(id);
        if (crs.isPresent()) {

            CourseType courseType = responseManagement.parseCourseType(type);

            // The actual Course Object
            Course course = crs.get();

            // Eine Liste aller SurveyResponses
            List<SurveyResponse> listOfSurveyResponses = responseManagement.findSurveyResponses(course, courseType,
                    group, instance);

            JSONArray resultsJsonString = responseManagement.generateSurveyResultsJson(listOfSurveyResponses);

            Integer totalNumberOfSubmissions = responseManagement.getTotalResponses(listOfSurveyResponses);

            model.addAttribute("resultsJson", resultsJsonString.toString());
            model.addAttribute("courseName", course.getName());
            model.addAttribute("courseType", courseType);
            model.addAttribute("semester", course.getCourseDate());
            model.addAttribute("numberOfSubmissions", totalNumberOfSubmissions);

        }
        return "surveyResults";
    }

    /**
     * The Mapping where students can browse the full set of Courses and View the
     * results of the student evaluations
     * 
     * @param model {@link org.springframework.ui.Model}
     * @return studentBrowser template
     */
    @GetMapping("studentBrowser")
    public String studentBrowser(Model model) {
        model.addAttribute("courseList", courseManagement.findAll());

        return "studentBrowser";
    }

    @GetMapping("surveyCheckout")
    public String surveyCheckout(Model model) {

        return "surveyCheckout";
    }

    // PDF Generation
    @GetMapping("/generatePDF")
    public HttpEntity<byte[]> generatePdf(@RequestParam UUID id, @RequestParam String type,
            @RequestParam String groupNumber, @RequestParam String instanceNumber, HttpServletResponse response)
            throws Exception {

        // generate filename
        String filename = "testPdf.pdf";

        Optional<Course> crs = courseManagement.findById(id);

        // verify that course is present
        if (crs.isEmpty()) {
            return null;
        }

        // Try to parse the courseType
        CourseType courseType = responseManagement.parseCourseType(type);
        if (courseType == null) {
            return null;
        }

        // Generate PDF
        byte[] pdf = responseManagement.generatePDFEnglish(crs.get(), courseType, Integer.parseInt(groupNumber),
                Integer.parseInt(instanceNumber));

        // Set HTTP headers and return HttpEntity
        HttpHeaders header = new HttpHeaders();
        header.setContentType(MediaType.APPLICATION_PDF);
        header.set(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + filename);
        header.setContentLength(pdf.length);

        return new HttpEntity<>(pdf, header);
    }

    // CSV Generation
    @GetMapping("/generateCSV")
    public HttpEntity<byte[]> generateCsv(@RequestParam UUID id, @RequestParam String type,
            @RequestParam String groupNumber, @RequestParam String instanceNumber, HttpServletResponse response)
            throws Exception {

        // generate filename
        String filename = "testCsv.csv";

        Optional<Course> crs = courseManagement.findById(id);

        // verify that course is present
        if (crs.isEmpty()) {
            return null;
        }

        // Try to parse the courseType
        CourseType courseType = responseManagement.parseCourseType(type);
        if (courseType == null) {
            return null;
        }

        // Generate CSV
        byte[] csv = responseManagement.generateCSVEnglish(
                responseManagement.findSurveyResponses(crs.get(), courseType, groupNumber, instanceNumber));

        if (csv.equals(new byte[0])) {
            return null;
        }

        // Set HTTP headers and return HttpEntity
        HttpHeaders header = new HttpHeaders();
        header.setContentType(MediaType.APPLICATION_PDF);
        header.set(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + filename);
        header.setContentLength(csv.length);

        return new HttpEntity<>(csv, header);
    }

    // test method
    @GetMapping("/createR")
    public String createR() {
        responseManagement.createTestResponses(courseManagement.findAll().iterator().next());
        return "home";
    }

    // CSV Generation
    @GetMapping("csv")
    public HttpEntity<byte[]> csvtest(HttpServletResponse response) throws Exception {

        Course crs = courseManagement.findAll().iterator().next();

        var group = "2";
        var instance = "2";

        // Eine Liste aller SurveyResponses
        List<SurveyResponse> listOfSurveyResponses = responseManagement.findSurveyResponses(crs, CourseType.TUTORIAL, group,
                instance);

        // Generate PDF
        byte[] pdf = responseManagement
                .generateCSVEnglish(listOfSurveyResponses);

        // Set HTTP headers and return HttpEntity
        HttpHeaders header = new HttpHeaders();
        header.setContentType(MediaType.APPLICATION_PDF);
        header.set(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + "csvTest.csv");
        header.setContentLength(pdf.length);

        return new HttpEntity<>(pdf, header);
    }

    @GetMapping("resultsTest")
    public String resultsTest(Model model) {

        Course course = courseManagement.findAll().iterator().next();

        CourseType courseType = CourseType.TUTORIAL;

        var group = "2";
        var instance = "2";

        // Eine Liste aller SurveyResponses
        List<SurveyResponse> listOfSurveyResponses = responseManagement.findSurveyResponses(course, courseType, group,
                instance);

        JSONArray resultsJsonString = responseManagement.generateSurveyResultsJson(listOfSurveyResponses);

        model.addAttribute("resultsJson", resultsJsonString);
        model.addAttribute("courseName", "Cheese 4 G's");
        model.addAttribute("courseType", "SEMINAR");
        model.addAttribute("semester", "WiSe 2020");
        model.addAttribute("numberOfSubmissions", "1935");

        return "surveyResults";
    }

    /*@GetMapping("resultsTest")
    public String resultsTest(Model model){

        //[{"type": "", "default": bool, "question": "", "options": [], "answers": []}, ...]}
        JSONArray results = new JSONArray();
        JSONObject question = new JSONObject();

        question.put("type", "text");
        question.put("default", true);
        question.put("question", "Is the earth flat?");

        ArrayList<String> options = new ArrayList<String>();
        options.add("A"); options.add("B"); options.add("C");
        question.put("answers", options);

        ArrayList<Double> answers = new ArrayList<Double>();
        answers.add(0.5); answers.add(0.5); answers.add(0.2);

        results.put(0, question);
        results.put(1, question);

        model.addAttribute("resultsJson", results.toString());
        model.addAttribute("courseName", "Cheese 4 G's");
        model.addAttribute("courseType", "SEMINAR");
        model.addAttribute("semester", "WiSe 2020");
        model.addAttribute("numberOfSubmissions", "1935");

        return "surveyResults";
    }*/

}
