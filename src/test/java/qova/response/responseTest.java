package qova.response;

import qova.AbstractIntegrationTest;
import qova.course.Course;
import qova.course.CourseFaculty;
import qova.course.CourseInstance;
import qova.course.CourseType;
import qova.responses.Response;
import qova.responses.ResponseType;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.time.LocalDate;
import java.util.ArrayList;

import org.junit.jupiter.api.Test;

public class responseTest extends AbstractIntegrationTest {
    @Test
    public void courseConstructorTest() throws Exception {

        var name = "Rechnernetze";

        String[] lectureTitles = {"Einführung" , "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2", "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance", "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"};
        var lecture = new CourseInstance(CourseType.LECTURE, 1, 12, lectureTitles);

        String[] tutorialTitles = {"Einführung" , "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2", "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance", "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"};
        var tutorial = new CourseInstance(CourseType.TUTORIAL, 8, 12, tutorialTitles);

        CourseInstance seminar = null;

        CourseInstance practical = null;

        var semesterOfStudents = 4;
        var faculty = CourseFaculty.COMPUTER_SCIENCE;
        var courseDate = LocalDate.of(2020, 10, 4);
        var semesterString = "SoSe 2020";

        Course crs  = new Course(name, lecture, tutorial, seminar, practical, semesterOfStudents, faculty, semesterString, courseDate);

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
            rspB = new Response(crs, courseType, position, classNo, responseTypeB, question, binaryAnswer);

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
            rspT = new Response(crs, courseType, position, classNo, responseTypeT, question, textResponse);

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
            rspM = new Response(crs, courseType, position, classNo, responseTypeM, question, responsePossibilites,
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
            rspD = new Response(crs, courseType, position, classNo, responseTypeD, question, responsePossibilites,
                    DDresponse, responseOptions);
                    
                assertEquals(true, rspD.getListMCDD().get(DDresponse));

        } 
        catch (Exception e) {
            e.printStackTrace();
        }

        
	}
}


