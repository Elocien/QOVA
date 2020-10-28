package qova.logic;

import java.io.IOException;
import java.util.*;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;

import com.google.zxing.WriterException;

import org.json.JSONArray;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

import qova.admin.AdminManagement;
import qova.admin.DefaultSurvey;
import qova.enums.CourseType;
import qova.forms.CourseForm;
import qova.forms.DuplicateCourseForm;
import qova.forms.InstanceTitleForm;
import qova.forms.SurveyForm;
import qova.objects.Course;
import qova.objects.CourseInstance;


@Controller
public class CourseController {

    @Autowired
    private final CourseManagement courseManagement;

    @Autowired
    private final ResponseManagement responseManagement;

    @Autowired
    private final AdminManagement adminManagement;

    @Autowired
    CourseController(CourseManagement courseManagement, ResponseManagement responseManagement,
            AdminManagement adminManagement) {
        this.courseManagement = Objects.requireNonNull(courseManagement);
        this.responseManagement = Objects.requireNonNull(responseManagement);
        this.adminManagement = Objects.requireNonNull(adminManagement);
    }

    // Error codes
    int courseNotFound = 1;

    // General Pages (relevant domain wide)
    // -------------------------------------------------------

    @GetMapping("/")
    public String welcome() {
        return "home";
    }

    // -------------------------------------------------------

    // Shows a table containing all courses
    @GetMapping("courses")
    @PreAuthorize("hasRole('ROLE_STAFF')")
    public String courses(Model model, Authentication authentication) {

        String userId = authentication.getPrincipal().toString();

        model.addAttribute("courseList", courseManagement.findByOwnerid(userId));
        return "courses";
    }

    // Shows the details for a specific course
    @PreAuthorize("hasRole('ROLE_STAFF')")
    @GetMapping("course/details")
    public String courseDetails(Model model, DuplicateCourseForm duplicateForm, CourseForm form,
            @RequestParam(required = false) UUID id) throws Exception {

        // for editing purposes:
        model.addAttribute("semesterDates", courseManagement.findSemesters());
        model.addAttribute("form", form);
        model.addAttribute("duplicateForm", duplicateForm);

        // fetch course and go to details if present
        Optional<Course> crs = courseManagement.findById(id);
        if (crs.isPresent()) {
            Course course = crs.get();

            model.addAttribute("course", course);

            //Used for status flags
            model.addAttribute("surveysMissing", courseManagement.getNumberOfSurveysMissing(course));
            model.addAttribute("titlesMissing", courseManagement.getNumberOfInstanceTitlesMissing(course));

            // QRCode URL (Redirects to a courses survey when scanned)
            String LectureSurveyUrl = "localhost:8080/surveySelect?type=LECTURE&id=" + course.getId()+"&mode=participant";
            String TutorialSurveyUrl = "localhost:8080/surveySelect?type=TUTORIAL&id=" + course.getId()+"&mode=participant";
            String SeminarSurveyUrl = "localhost:8080/surveySelect?type=SEMINAR&id=" + course.getId()+"&mode=participant";
            String PracticalSurveyUrl = "localhost:8080/surveySelect?type=PRACTICAL&id=" + course.getId()+"&mode=participant";

            model.addAttribute("lectureLink", LectureSurveyUrl);
            model.addAttribute("tutorialLink", TutorialSurveyUrl);
            model.addAttribute("seminarLink", SeminarSurveyUrl);
            model.addAttribute("practicalLink", PracticalSurveyUrl);

            // send byte array (the QRCode image) to model
            model.addAttribute("lectureQRCode", Base64.getEncoder().encodeToString(courseManagement.generateQRCodeImage(LectureSurveyUrl)));
            model.addAttribute("tutorialQRCode", Base64.getEncoder().encodeToString(courseManagement.generateQRCodeImage(TutorialSurveyUrl)));
            model.addAttribute("seminarQRCode", Base64.getEncoder().encodeToString(courseManagement.generateQRCodeImage(SeminarSurveyUrl)));
            model.addAttribute("practicalQRCode", Base64.getEncoder().encodeToString(courseManagement.generateQRCodeImage(PracticalSurveyUrl)));

            return "courseDetails";
        } else {
            return "redirect:/error?code=" + courseNotFound;
        }
    }

    // Edit Course Validation (when course is updated, check wether the fields are
    // all appropriately set e.g. NotNull)
    @PostMapping("course/edit")
    public String editCourseValidation(Model model, @Valid @ModelAttribute("form") CourseForm form,
            BindingResult result, DuplicateCourseForm duplcateCourseForm, @RequestParam UUID id) throws Exception {

        if (result.hasErrors()) {
            return courseDetails(model, duplcateCourseForm, form, id);
        }

        Optional<Course> crs = courseManagement.findById(id);
        if (crs.isPresent()) {
            if (Boolean.TRUE.equals(crs.get().getFinalisedFlag())) {
                return courseDetails(model, duplcateCourseForm, form, id);
            }
            courseManagement.updateCourseDetails(id, form);
            return "redirect:../course/details?id=" + id;
        }

        return "redirect:/";

    }

    // Create Course
    @GetMapping("course/new")
    public String createCourse(Model model, CourseForm form) {

        model.addAttribute("form", form);

        // List of Semesters for Course Creator to pick from
        model.addAttribute("semesterDates", courseManagement.findSemesters());
        return "courseNew";
    }

    // Validation of Created course
    @PostMapping("course/new")
    public String createCourseValidation(Model model, @Valid @ModelAttribute("form") CourseForm form,
                                         BindingResult result, Authentication authentication) {

        if (result.hasErrors()) {
            return createCourse(model, form);
        }

        // Get DefaultSurvey to reference in CourseInstance
        EnumMap<CourseType, DefaultSurvey> defaultSurveyMap = new EnumMap<>(CourseType.class);
        for(CourseType courseType : CourseType.values()){
            defaultSurveyMap.put(courseType, adminManagement.getDefaultSurveyObject(courseType));
        }

        String userId = authentication.getPrincipal().toString();

        if(userId.isEmpty()){
            return "redirect:/";
        }

        // Management Method returns String of new Course
        UUID id = courseManagement.createCourseAndCourseInstanceAndReturnCourseId(userId, form, defaultSurveyMap);

        // Redirect to SurveyEditor to start creating survey
        return "redirect:../course/instanceTitles?id=" + id;
    }

    // Create Course
    @GetMapping("course/instanceTitles")
    public String createCourseSetInstanceTitles(Model model, @ModelAttribute("form") InstanceTitleForm form,
            @RequestParam UUID id) {

        Optional<Course> crs = courseManagement.findById(id);

        if (crs.isPresent()) {

            Course course = crs.get();

            model.addAttribute("id", crs.get().getId());

            if (Boolean.TRUE.equals(crs.get().getLectureExists())) {
                model.addAttribute("lectureExists", true);
                model.addAttribute("lectureInstances", course.getLecture().getInstanceAmount());
                model.addAttribute("lectureInstanceTitles", course.getLecture().getInstanceTitles());
            } else {
                model.addAttribute("lectureExists", false);
            }

            if (Boolean.TRUE.equals(crs.get().getTutorialExists())) {
                model.addAttribute("tutorialExists", true);
                model.addAttribute("tutorialInstances", course.getTutorial().getInstanceAmount());
                model.addAttribute("tutorialInstanceTitles", course.getTutorial().getInstanceTitles());
            } else {
                model.addAttribute("tutorialExists", false);
            }

            if (Boolean.TRUE.equals(crs.get().getSeminarExists())) {
                model.addAttribute("seminarExists", true);
                model.addAttribute("seminarInstances", course.getSeminar().getInstanceAmount());
                model.addAttribute("seminarInstanceTitles", course.getSeminar().getInstanceTitles());
            } else {
                model.addAttribute("seminarExists", false);
            }

            if (Boolean.TRUE.equals(crs.get().getPracticalExists())) {
                model.addAttribute("practicalExists", true);
                model.addAttribute("practicalInstances", course.getPractical().getInstanceAmount());
                model.addAttribute("practicalInstanceTitles", course.getPractical().getInstanceTitles());
            } else {
                model.addAttribute("practicalExists", false);
            }
        }

        return "instanceTitles";
    }

    // Validation of Created course
    @PostMapping("course/instanceTitles")
    public String createCourseSetInstanceTitlesValidation(Model model, InstanceTitleForm form, @RequestParam UUID id,
            BindingResult result) {

        // Management Method returns String of new Course
        courseManagement.createCourseSetInstanceTitles(form, id);

        // Redirect to SurveyEditor to start creating survey
        return "redirect:../course/details" + "?id=" + id;
    }

    // Delete Course and its CourseInstances
    @GetMapping("course/delete")
    public String deleteCourse(@RequestParam UUID id) {
        courseManagement.deleteCourse(id);
        return "redirect:../courses";
    }

    @PostMapping("course/duplicate")
    public String duplicateCourseWithNewSemester(@ModelAttribute("duplicateForm") DuplicateCourseForm form,
            @RequestParam UUID id) {

        Course newCourse = courseManagement.duplicateCourse(id, form.getSemesterString());

        return "redirect:../course/details?id=" + newCourse.getId();
    }

    @GetMapping("course/finalise")
    public String finaliseCourse(@RequestParam UUID id) {

        courseManagement.setCourseFinalised(id);

        Optional<Course> course = courseManagement.findById(id);
        if (course.isPresent()) {
            for (CourseType courseType : CourseType.values()) {

                String completeSurvey = adminManagement.concatenateDefaultSurveyToSurveyString(
                        courseManagement.getSurveyforType(id, courseType), courseType);

                try {
                    new JSONArray(completeSurvey);
                } catch (Exception e) {
                    return "redirect:../course/details" + "?id=" + id;
                }

                // Create JSON Array
                JSONArray survey = new JSONArray(completeSurvey);

                // Create the relevant objects
                responseManagement.createSurveyResponse(survey, course.get(), courseType);
            }
        }

        return "redirect:../course/details?id=" + id;
    }

    // Call Questioneditor and Submit created Survey
    // ---------------------------------------------------------------------------

    /**
     * Mapping for surveyeditor HTML (called from CourseDetails Page!)
     *
     * @param model {@link org.springframework.ui.Model}
     * @param type  {@linkplain qova.enums.CourseType} in String form
     * @param id    Id of the Course
     * @return questioneditor.html template
     */
    @GetMapping("course/surveyeditor")
    public String questioneditor(Model model, @RequestParam String type, @RequestParam(required = false) UUID id) {

        // Give model the following attributes, which are used to submit the survey, via
        // the post method
        model.addAttribute("typeID", type);
        model.addAttribute("id", id);

        // Gives the survey JSON to the model, so the current survey can be assembled
        // and added to
        model.addAttribute("survey", courseManagement.getSurveyforType(id, responseManagement.parseCourseType(type)));

        // Default survey JSON, which is sent to the server
        model.addAttribute("defaultSurvey", adminManagement.getDefaultSurvey(responseManagement.parseCourseType(type)));

        // give course name to model, to show as title
        Optional<Course> course = courseManagement.findById(id);
        model.addAttribute("coursename", course.get().getName());

        return "questioneditor";
    }

    /**
     * Mapping for submitting a created survey. The questioneditor sends JSON
     * containing the survey to the server, and this is checked for length (Can't
     * exceed 100 questions) and special characters. If the
     * {@linkplain qova.objects.CourseInstance} hasn't had a survey set before, then
     * the flag is set for that given {@linkplain qova.objects.CourseInstance}
     *
     * @param model The {@linkplain org.springframework.ui.Model}
     * @param form  {@linkplain SurveyForm} which contains the JSON passed by the
     *              surveyeditor
     * @param type  {@linkplain qova.enums.CourseType} in String format
     * @param id    Id of the {@linkplain Course}
     * @return Either the errorPage, in case of error; otherwise return the
     *         courseDetails template
     */
    @PostMapping("course/surveyeditor")
    public String questioneditorSubmit(Model model, @Valid @ModelAttribute("form") SurveyForm form,
            @RequestParam String type, @RequestParam(required = false) UUID id) {

        // Form empty -> Redirect to details again
        if (form.getQuestionnaireJson().length() == 0) {
            return "redirect:../course/details" + "?id=" + id;
        }

        // fetch course
        Optional<Course> course = courseManagement.findById(id);
        if (course.isPresent()) {

            CourseType courseType = responseManagement.parseCourseType(type);

            // if type is none of the correct values, then redirect to error page
            if (courseType == null) {
                return "error";
            }

            CourseInstance instance = course.get().getInstance(courseType);

            if (Boolean.FALSE.equals(instance.getSurveyEditedFlag())) {
                courseManagement.setSurveyEditedFlagForCourseInstance(instance);
            }

            else {
                // check if JSON is valid
                try {
                    new JSONArray(form.getQuestionnaireJson());
                } catch (Exception e) {
                    return questioneditor(model, type, id);
                }

                // Create a JSON Array out of the response from the questioneditor
                JSONArray survey = new JSONArray(form.getQuestionnaireJson());

                // parse JSON to check for correctness (length, special characters)
                Boolean validSurvey = responseManagement.verifyJsonArray(survey);
                if (Boolean.FALSE.equals(validSurvey)) {
                    return questioneditor(model, type, id);
                }

                // Sets the survey string for a given course (takes the default survey and
                // conncatenates it with the create survey)
                courseManagement.setSurveyforType(course.get(), type, form.getQuestionnaireJson());
            }

            // Redirect back to CourseDetails page
            return "redirect:../course/details" + "?id=" + id;
        } else {
            return "error";
        }
    }

    // ---------------------------------------------------------------------------

    /**
     * The GetMapping to preview a survey created in the SurveyEditor
     *
     * @param model The {@linkplain Model}
     * @param type The {@linkplain CourseType}
     * @param id The id of the {@linkplain Course}
     * @return The surveyPreview template
     */
    @GetMapping("course/previewsurvey")
    public String questioneditorpreviewget(Model model, @RequestParam String type,
                                           @RequestParam(required = false) UUID id) {

        // fetch course
        Optional<Course> course = courseManagement.findById(id);
        if (course.isPresent()) {

            // if type is none of the correct values, then redirect to homepage
            if (responseManagement.parseCourseType(type) == null) {
                return "error";
            }

            model.addAttribute("typeID", type);
            model.addAttribute("id", id);
            model.addAttribute("survey",
                    courseManagement.getSurveyforType(id, responseManagement.parseCourseType(type)));
            model.addAttribute("defaultSurvey",
                    adminManagement.getDefaultSurvey(responseManagement.parseCourseType(type)));
            model.addAttribute("coursename", course.get().getName());

            return "surveypreview";
        } else {
            return "error";
        }
    }

    /**
     * The PostMapping for the surveyPreview. This functions the same as
     * {@linkplain CourseController#questioneditorSubmit(Model, SurveyForm, String, UUID)}. It sets the survey string
     * as well as the surveyEdited flag.
     *
     * @param model The {@linkplain Model}
     * @param type The {@linkplain CourseType}
     * @param id The id of the {@linkplain Course}
     * @param form {@linkplain SurveyForm} containing the surveyJson from the submission
     * @return The surveyPreview template
     */
    @PostMapping("course/previewsurvey")
    public String questioneditorpreview(Model model, SurveyForm form, @RequestParam String type,
            @RequestParam(required = false) UUID id) {

        // Form empty -> Redirect to details again
        if (form.getQuestionnaireJson().length() == 0) {
            return "redirect:../course/details" + "?id=" + id;
        }

        // fetch course
        Optional<Course> course = courseManagement.findById(id);
        if (course.isPresent()) {

            CourseType courseType = responseManagement.parseCourseType(type);

            // if type is none of the correct values, then redirect to homepage
            if (courseType == null) {
                return "error";
            }

            CourseInstance instance = course.get().getInstance(courseType);

            //
            if (Boolean.FALSE.equals(instance.getSurveyEditedFlag())) {
                courseManagement.setSurveyEditedFlagForCourseInstance(instance);
            }

            else {
                // check if JSON is valid
                try {
                    new JSONArray(form.getQuestionnaireJson());
                } catch (Exception e) {
                    return questioneditor(model, type, id);
                }

                // Create a JSON Array out of the response from the questioneditor
                JSONArray survey = new JSONArray(form.getQuestionnaireJson());

                // parse JSON to check for correctness (length, special characters)
                Boolean validSurvey = responseManagement.verifyJsonArray(survey);
                if (Boolean.FALSE.equals(validSurvey)) {
                    // TODO: redirect to error page with code 02
                    return questioneditor(model, type, id);
                }

                // Sets the survey string for a given course (takes the default survey and
                // conncatenates it with the create survey)
                courseManagement.setSurveyforType(course.get(), type, form.getQuestionnaireJson());
            }

            // Part der anders ist als questioneditorSubmit
            model.addAttribute("typeID", responseManagement.parseCourseType(type));
            model.addAttribute("id", id);

            return "redirect:../course/previewsurvey?id=" + id + "&type=" + type;
        } else {
            // TODO: need more feedback here for the user. Change this!
            return "error?code=" + courseNotFound;
        }
    }


    /**
     * This method takes id and CourseType as parameters, and returns a qrcode with
     * the given string that is assembled below
     *
     * @param response {@link javax.servlet.http.HttpServletResponse}
     * @param type     {@linkplain qova.enums.CourseType}
     * @param id       Id of the {@linkplain Course}
     * @return The QRCode as a byte[], bundled into a {@link HttpEntity}
     * @throws IOException     Thrown by QRCode generator
     * @throws WriterException Thrown by QRCode generator
     */
    @GetMapping("qrcode")
    public HttpEntity<byte[]> qrcode(HttpServletResponse response, @RequestParam String type, @RequestParam UUID id)
            throws IOException, WriterException {

        // QRCode URL (Redirects to a courses survey when scanned). Generated using
        // pathvariables
        String url = "localhost:8080/survey?type=" + type + "&id=" + id;

        // find course
        Optional<Course> crs = courseManagement.findById(id);

        // generate filename
        String filename = crs.get().getName() + "_" + type + "_" + "QRCode";

        // Generate QRCode
        byte[] qrcode = courseManagement.generateQRCodeImage(url);

        // Set HTTP headers and return HttpEntity
        HttpHeaders header = new HttpHeaders();
        header.setContentType(MediaType.IMAGE_PNG);
        header.set(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + filename);
        header.setContentLength(qrcode.length);

        return new HttpEntity<byte[]>(qrcode, header);
    }

}
