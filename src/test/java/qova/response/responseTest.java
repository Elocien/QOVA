package qova.response;

import qova.AbstractIntegrationTest;
import qova.course.Course;
import qova.course.CourseFaculty;
import qova.course.CourseType;
import qova.responses.Response;
import qova.responses.ResponseType;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.time.LocalDate;
import java.util.ArrayList;

import org.junit.jupiter.api.Test;

public class responseTest extends AbstractIntegrationTest {
    @Test
    public void courseConstructorTest() {

        var name = "test";
        var lectureExists = true;
        var tutorialExists = false;
        var seminarExists = true;
        var lectureSurvey = "";
        var tutorialSurvey = "";
        var seminarSurvey = "";
        var classTotalSeminar = 10;
        var classTotalTutorial = 5;
        var semesterOfStudents = 6;
        var faculty = CourseFaculty.COMPUTER_SCIENCE;
        var courseInstance = LocalDate.of(2020, 10, 4);
        var semesterUI = "SoSe 2020";
        Course course = new Course(name, lectureExists, tutorialExists, seminarExists, lectureSurvey, tutorialSurvey,
                seminarSurvey, classTotalTutorial, classTotalSeminar, semesterOfStudents, faculty, semesterUI,
                courseInstance);

        // ----------------------------------------------------------------------------------------------------------------------

        var courseType = CourseType.LECTURE;
        var position = 1;
        var classNo = 1;
        var question = "test";
        var responsePossibilites = 3;

        // create test response (binaryAnswer constructor)
        var binaryAnswer = true;
        var responseTypeB = ResponseType.BINARY_ANSWER;
        Response rspB;

        try {
            rspB = new Response(course, courseType, position, classNo, responseTypeB, question, binaryAnswer);

            assertEquals(responseTypeB, rspB.getResponseType());
            assertEquals(binaryAnswer, rspB.getBinaryAnswer());

            // Common parameters
            assertEquals(courseType, rspB.getCourseType());
            assertEquals(position, rspB.getPosition());
            assertEquals(classNo, rspB.getClassNo());
            assertEquals(question, rspB.getQuestion());

        } catch (Exception e) {
            e.printStackTrace();
        }

        // create test response (TextResponse constructor)
        var responseTypeT = ResponseType.TEXT_RESPONSE;
        var textResponse = "some response";

        Response rspT;
        try {
            rspT = new Response(course, courseType, position, classNo, responseTypeT, question, textResponse);

            assertEquals(responseTypeT, rspT.getResponseType());
            assertEquals(textResponse, rspT.getTextResponse());

        } catch (Exception e) {
            e.printStackTrace();
        }

        // create test response (MultipleChoice constructor)

        var responseTypeM = ResponseType.MULTIPLE_CHOICE;
        ArrayList<Boolean> MCresponse = new ArrayList<Boolean>();
        MCresponse.add(false);
        MCresponse.add(true);
        MCresponse.add(false);

        ArrayList<String> responseOptions = new ArrayList<String>();
        responseOptions.add("option1");
        responseOptions.add("option2");
        responseOptions.add("option3");

        Response rspM;
        try {
            rspM = new Response(course, courseType, position, classNo, responseTypeM, question, responsePossibilites,
                    MCresponse, responseOptions);

            assertEquals(MCresponse, rspM.getListMCDD());
            assertEquals(responseOptions, rspM.getOptionsMCDD());

        } catch (Exception e) {
            e.printStackTrace();
        }

        // create test response (MultipleChoice constructor)

        var responseTypeD = ResponseType.SINGLE_CHOICE;
        var DDresponse = 1;

        Response rspD;
        try {
            rspD = new Response(course, courseType, position, classNo, responseTypeD, question, responsePossibilites,
                    DDresponse, responseOptions);
                    
                assertEquals(true, rspD.getListMCDD().get(DDresponse));

        } 
        catch (Exception e) {
            e.printStackTrace();
        }

        
	}
}


