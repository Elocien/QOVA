package qova.logic;

import java.io.IOException;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
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
    ResponseController(ResponseManagement responseManagement, CourseManagement courseManagement,
            AdminManagement adminManagement) {
        this.responseManagement = Objects.requireNonNull(responseManagement);
        this.courseManagement = Objects.requireNonNull(courseManagement);
        this.adminManagement = Objects.requireNonNull(adminManagement);
    }

    // Error codes
    int courseNotFound = 1;
    int internalError = 2;

    // Mapping to which one is redirected to by the QRCode. This is where students
    // enter which group and which topic they are handing their response in for
    // ---------------------------------------------------------------------------

    @GetMapping("surveySelect")
    public String selectSurvey(Model model, SurveySelectForm form, @RequestParam UUID id, @RequestParam String type) {

        // course name, course type, instance names, groupAmount
        Optional<Course> crs = courseManagement.findById(id);
        if (crs.isPresent()) {
            model.addAttribute("courseName", crs.get().getName());
            model.addAttribute("courseType", type);
            model.addAttribute("id", crs.get().getId());
            model.addAttribute("form", form);

            if (type.equals("LECTURE")) {
                model.addAttribute("instanceTitles", crs.get().getLecture().getInstanceTitles());
                model.addAttribute("groupAmount", crs.get().getLecture().getGroupAmount());
            }
            if (type.equals("TUTORIAL")) {
                model.addAttribute("instanceTitles", crs.get().getTutorial().getInstanceTitles());
                model.addAttribute("groupAmount", crs.get().getTutorial().getGroupAmount());
            }
            if (type.equals("SEMINAR")) {
                model.addAttribute("instanceTitles", crs.get().getSeminar().getInstanceTitles());
                model.addAttribute("groupAmount", crs.get().getSeminar().getGroupAmount());
            }
            if (type.equals("PRACTICAL")) {
                model.addAttribute("instanceTitles", crs.get().getPractical().getInstanceTitles());
                model.addAttribute("groupAmount", crs.get().getPractical().getGroupAmount());
            }
            return "surveySelect";
        }

        // if course does not exist, redirect to global error page
        return "error?code=" + courseNotFound;
    }

    // ---------------------------------------------------------------------------

    // TODO: rename path variables
    // Validation of entry of surveySelect page, and redirect to the actual survey
    @PostMapping("surveySelect")
    public String selectSurveySubmission(Model model, @ModelAttribute("form") SurveySelectForm form,
            @RequestParam UUID id, @RequestParam String type) {

        Optional<Course> crs = courseManagement.findById(id);

        // if anything is null or not an allowed value, redirect back
        if (!crs.isPresent()) {
            return "error?code=" + courseNotFound;
        }
        // if type is not one of the defined values
        if (!(type.equals("LECTURE")) && !(type.equals("TUTORIAL")) && !(type.equals("SEMINAR"))
                && !(type.equals("PRACTICAL"))) {
            return "error?code=" + internalError;
        }

        // TODO validate that parameters only contain valid charachters. E.g. a-zA-Z0-9

        else {
            return "survey?type=" + type + "&id=" + id + "instanceTitle=" + form.getInstance() + "groupNumber="
                    + form.getGroup();
        }
    }

    @GetMapping("surveySelectAdmin")
    public String surveSelectAdmin(Model model, @RequestParam UUID id) {
        Optional<Course> crs = courseManagement.findById(id);
        if (crs.isPresent()) {
            model.addAttribute("course", crs.get());
            return "surveySelectAdmin";
        }

        // if course does not exist, redirect to global error page
        return "error?code=" + courseNotFound;
    }

    // Get Survey from Server
    // ---------------------------------------------------------------------------

    // Mapping for Survey HTML
    @GetMapping("survey")
    public String SurveyView(Model model, @RequestParam(required = false) UUID id,
            @RequestParam(required = false) String type, @RequestParam(required = false) String group,
            @RequestParam(required = false) String instance) {
        // redirect
        if (id == null || type == null || group == null || instance == null) {
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

                // Concatenate the default survey to the course survey
                survey = adminManagement.concatenateDefaultSurveyToSurveyString(survey,
                        responseManagement.parseCourseType(type));

                model.addAttribute("typeID", type);
                model.addAttribute("id", id);
                model.addAttribute("survey", survey);
                model.addAttribute("coursename", course.get().getName());
                return "survey";
            }

        }

        // If condition not met, redirect to home
        else {
            return "error?code=" + courseNotFound;
        }
    }

    // PostMapping to submit survey and serialize results
    // ---------------------------------------------------------------------------
    @PostMapping("/survey")
    public String recieveResponseJSON(Model model, SurveyForm form, @RequestParam(required = false) UUID id,
            @RequestParam(required = false) String type, @RequestParam(required = false) String group,
            @RequestParam(required = false) String instance) {

        if (id == null || type == null || group == null || instance == null) {
            return "error?code=" + courseNotFound;
        }

        JSONArray studentResponseJson;

        // get JSON Response
        try {
            studentResponseJson = new JSONArray(form.getQuestionnairejson());
        } catch (Exception e) {
            return "error?code=" + internalError;
        }

        // fetch course and go to details if present
        Optional<Course> crs = courseManagement.findById(id);

        // Validate that course exists, and that the survey is not empty
        if (crs.isPresent()) {

            Course course = crs.get();

            Optional<SurveyResponse> survRsp = responseManagement
                    .findSurveyResponseByCourseAndCourseTypeAndGroupNumberAndInstanceNumber(course,
                            responseManagement.parseCourseType(type), Integer.valueOf(group),
                            Integer.valueOf(instance));

            if (survRsp.isPresent()) {
                SurveyResponse surveyResponse = survRsp.get();

            }

        }

        // Manager Method
        // Increment numberOfSubmissions in SurveyResponse
        // Add stundent ID to SurveyResponse List

        // if all goes well
        return "postSubmissionLanding";
    }

    // ---------------------------------------------------------------------------

    /**
     * Used to retrieve the results for a given questionnaire
     * 
     * @param model    {@link org.springframework.ui.Model}
     * @param type     {@linkplain qova.enums.CourseType}
     * @param id       The Id of the {@linkplain qova.objects.Course}
     * @param group    The groupNumber of the {@linkplain qova.objects.Course}
     * @param instance The instanceNumber of the {@linkplain qova.objects.Course}
     * @return The surveyResults template, which shows the compiled results of the
     *         requested questionnaire
     */
    @GetMapping("/surveyresults")
    public String surveyResultsTest(Model model, @RequestParam String type, @RequestParam UUID id,
            @RequestParam String group, @RequestParam String instance) {
        Optional<Course> crs = courseManagement.findById(id);
        if (crs.isPresent()) {
            Optional<SurveyResponse> surveyResponse = responseManagement
                    .findSurveyResponseByCourseAndCourseTypeAndGroupNumberAndInstanceNumber(crs.get(),
                            responseManagement.parseCourseType(type), Integer.valueOf(group),
                            Integer.valueOf(instance));

            if (surveyResponse.isPresent()) {
                model.addAttribute("response", surveyResponse);
            }
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

    // PDF Generation
    @GetMapping("/generatePDF")
    public HttpEntity<byte[]> generatePdf(@RequestParam UUID id, @RequestParam String type,
            @RequestParam String groupNumber, @RequestParam String instanceNumber, HttpServletResponse response)
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
    public HttpEntity<byte[]> generateCsv(@RequestParam UUID id, @RequestParam String type,
            @RequestParam String groupNumber, @RequestParam String instanceNumber, HttpServletResponse response)
            throws Exception {

        // generate filename
        String filename = "testCsv.csv";

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
        byte[] csv = responseManagement.generateCSV_en(crs.get(), courseType, groupNumber, instanceNumber);

        if (csv == new byte[0]) {
            return null;
        }

        // Set HTTP headers and return HttpEntity
        HttpHeaders header = new HttpHeaders();
        header.setContentType(MediaType.APPLICATION_PDF);
        header.set(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + filename);
        header.setContentLength(csv.length);

        return new HttpEntity<byte[]>(csv, header);
    }

    // test method
    @GetMapping("/createR")
    public String createR() {
        responseManagement.createTestResponses(courseManagement.findAll().iterator().next());
        return "home";
    }

    // PDF Generation
    @GetMapping("/pdftest")
    public HttpEntity<byte[]> pdfTest(HttpServletResponse response) throws Exception {

        // generate filename
        String filename = "testPdf.pdf";

        // Generate PDF
        byte[] pdf = responseManagement.generatePDF_test();

        // Set HTTP headers and return HttpEntity
        HttpHeaders header = new HttpHeaders();
        header.setContentType(MediaType.APPLICATION_PDF);
        header.set(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + filename);
        header.setContentLength(pdf.length);

        return new HttpEntity<byte[]>(pdf, header);
    }

    // CSV Generation
    @GetMapping("csv")
    public HttpEntity<byte[]> csvtest(HttpServletResponse response) throws Exception {

        Course crs = courseManagement.findAll().iterator().next();

        // Generate PDF
        byte[] pdf = responseManagement.generateCSV_en(crs, CourseType.TUTORIAL, "1", "all");

        // Set HTTP headers and return HttpEntity
        HttpHeaders header = new HttpHeaders();
        header.setContentType(MediaType.APPLICATION_PDF);
        header.set(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + "csvTest.csv");
        header.setContentLength(pdf.length);

        return new HttpEntity<byte[]>(pdf, header);
    }

    // @GetMapping("/surveyResults")
    // public String surveyResults(Model model) throws Exception {

    // Course course = courseManagement.TimTestCreateCourse();
    // SurveyResponse rsp = responseManagement.timCreateTestResponses(course);

    // model.addAttribute("response", rsp);

    // return "surveyResults";
    // }

}
