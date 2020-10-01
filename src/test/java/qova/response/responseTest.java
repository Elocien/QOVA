package qova.response;

import qova.AbstractIntegrationTest;
import qova.enums.CourseType;
import qova.objects.AbstractResponse;
import qova.objects.BinaryResponse;
import qova.objects.Course;
import qova.objects.MultipleChoiceResponse;
import qova.objects.SingleChoiceResponse;
import qova.objects.SurveyResponse;
import qova.objects.TextResponse;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

public class responseTest extends AbstractIntegrationTest {

    @Test
    public void UserResponseConstructorTest() throws Exception {

        // ----------------------------------------------------------------------------------------------------------------------
        Course course = Mockito.mock(Course.class);

        var type = CourseType.LECTURE;
        var instance = 1;
        var group = 1;

        List<AbstractResponse> listOfResponses = new ArrayList<>();

        var binaryResponseQuestion = "Would you consider recommending the lecture to other students?";
        BinaryResponse bnr = new BinaryResponse(binaryResponseQuestion, 0, true);
        for (int i = 0; i < 35; i++) {
            bnr.incrementYes();
        }
        for (int i = 0; i < 15; i++) {
            bnr.incrementNo();
        }

        listOfResponses.add(bnr);

        List<String> mcOptions = new ArrayList<>();
        mcOptions.add("1");
        mcOptions.add("2");
        mcOptions.add("3");
        mcOptions.add("4");
        mcOptions.add("5");
        var multipleChoiceResponseQuestion = "From 1 to 5, what would you rate the lecture?";
        MultipleChoiceResponse mcr = new MultipleChoiceResponse(multipleChoiceResponseQuestion, 1, mcOptions, true);

        List<Integer> mcAnswers1 = new ArrayList<>();
        mcAnswers1.add(0);
        mcAnswers1.add(1);
        mcAnswers1.add(3);

        List<Integer> mcAnswers2 = new ArrayList<>();
        mcAnswers2.add(1);
        mcAnswers2.add(4);

        List<Integer> mcAnswers3 = new ArrayList<>();
        mcAnswers3.add(2);
        mcAnswers3.add(4);

        for (int i = 0; i < 25; i++) {
            mcr.incrementTotals(mcAnswers1);
        }
        for (int i = 0; i < 15; i++) {
            mcr.incrementTotals(mcAnswers2);
        }
        for (int i = 0; i < 10; i++) {
            mcr.incrementTotals(mcAnswers3);
        }

        var textResponseQuestion = "What is your opinion of the lecture, is it helpful?";
        TextResponse txr = new TextResponse(textResponseQuestion, 2, false);
        for (int i = 0; i < 20; i++) {
            txr.addTextSubmission("this is a bit of a test");
        }
        for (int i = 0; i < 10; i++) {
            txr.addTextSubmission("this is a larger test to test the test");
        }
        for (int i = 0; i < 17; i++) {
            txr.addTextSubmission("short test");
        }
        for (int i = 0; i < 3; i++) {
            txr.addTextSubmission(
                    "this is a very very very very very very very very very very very very very very very very very very very very large test");
        }

        SurveyResponse response = new SurveyResponse(course, type, instance, group, listOfResponses);

        List<String> listOfStudentIds = new ArrayList<>();
        for (int i = 0; i < 50; i++) {
            String id = UUID.randomUUID().toString();
            response.addStundentIdToSubmissionListAndIncrementCounter(id);
            listOfStudentIds.add(id);
        }

        assertEquals(course, response.getCourse());
        assertEquals(type, response.getCourseType());
        assertEquals(group, response.getGroupNumber());
        assertEquals(instance, response.getInstanceNumber());
        // The Maps order is not persisted in the same order the objects are added to it
        assertEquals(listOfStudentIds.containsAll(response.getListOfStudentsThatSubmitted()),
                response.getListOfStudentsThatSubmitted().containsAll(listOfStudentIds));
        assertEquals(50, response.getNumberOfSubmissions());
        assertEquals(listOfResponses, response.getListOfResponses());
    }

    @Test
    public void BinaryResponseConstructorTest() {

        var question = "Was the lecture informative";
        var surveyPosition = 1;
        var isDefaultQuestion = true;

        BinaryResponse br = new BinaryResponse(question, surveyPosition, isDefaultQuestion);
        br.incrementYes();
        br.incrementNo();
        br.incrementNo();

        assertEquals(question, br.getQuestion());
        assertEquals("1", br.getYesTotalString());
        assertEquals("2", br.getNoTotalString());
        assertEquals(1, br.getYesTotal());
        assertEquals(2, br.getNoTotal());
        assertEquals(3, br.getTotal());
        assertEquals(qova.enums.ResponseType.BINARY_ANSWER, br.getType());
        assertEquals(surveyPosition, br.getSurveyPosition());
        assertEquals(isDefaultQuestion, br.getIsDefaultQuestion());
    }

    @Test
    public void TextResponseConstructorTest() {

        var question = "Was the lecture informative";
        var surveyPosition = 10;
        var isDefaultQuestion = false;

        TextResponse tr = new TextResponse(question, surveyPosition, isDefaultQuestion);

        assertEquals(question, tr.getQuestion());
        assertEquals(qova.enums.ResponseType.TEXT_RESPONSE, tr.getType());
        assertEquals(surveyPosition, tr.getSurveyPosition());
        assertEquals(isDefaultQuestion, tr.getIsDefaultQuestion());
    }

    @Test
    public void SingleChoiceConstructorTest() throws Exception {

        String question = "Rate the lecutre from 1 to 5";
        var surveyPosition = 5;
        var isDefaultQuestion = false;

        ArrayList<String> scOptions = new ArrayList<String>();
        scOptions.add("1");
        scOptions.add("2");
        scOptions.add("3");
        scOptions.add("4");
        scOptions.add("5");
        SingleChoiceResponse scr = new SingleChoiceResponse(question, surveyPosition, scOptions, isDefaultQuestion);

        for (int i = 0; i < 3; i++) {
            scr.incrementTotal(0);
        }
        for (int i = 0; i < 8; i++) {
            scr.incrementTotal(1);
        }
        for (int i = 0; i < 16; i++) {
            scr.incrementTotal(2);
        }
        for (int i = 0; i < 15; i++) {
            scr.incrementTotal(3);
        }
        for (int i = 0; i < 8; i++) {
            scr.incrementTotal(4);
        }

        ArrayList<Integer> totals = new ArrayList<Integer>();
        totals.add(3);
        totals.add(8);
        totals.add(16);
        totals.add(15);
        totals.add(8);

        assertEquals(question, scr.getQuestion());
        assertEquals(scOptions, scr.getSingleChoiceOptions());
        assertEquals(totals, scr.getSingleChoiceAnswers());
        assertEquals(qova.enums.ResponseType.SINGLE_CHOICE, scr.getType());
        assertEquals(surveyPosition, scr.getSurveyPosition());
        assertEquals(isDefaultQuestion, scr.getIsDefaultQuestion());
    }

    @Test
    public void MultipleChoiceConstructorTest() throws Exception {

        String question = "What was good about the lecture (multiple options can be selected)";
        var surveyPosition = 5;
        var isDefaultQuestion = false;

        ArrayList<String> mcOptions = new ArrayList<String>();
        mcOptions.add("It was informative");
        mcOptions.add("It was interesting");
        mcOptions.add("I learned something new");
        mcOptions.add("I enjoyed attending the lecture");
        mcOptions.add("I would recommend the lecture to others");
        MultipleChoiceResponse mcr = new MultipleChoiceResponse(question, surveyPosition, mcOptions, isDefaultQuestion);

        ArrayList<Integer> mcAnswers1 = new ArrayList<Integer>();
        mcAnswers1.add(0);
        mcAnswers1.add(1);
        mcAnswers1.add(3);

        ArrayList<Integer> mcAnswers2 = new ArrayList<Integer>();
        mcAnswers2.add(1);
        mcAnswers2.add(4);

        ArrayList<Integer> mcAnswers3 = new ArrayList<Integer>();
        mcAnswers3.add(2);
        mcAnswers3.add(4);

        for (int i = 0; i < 25; i++) {
            mcr.incrementTotals(mcAnswers1);
        }
        for (int i = 0; i < 15; i++) {
            mcr.incrementTotals(mcAnswers2);
        }
        for (int i = 0; i < 10; i++) {
            mcr.incrementTotals(mcAnswers3);
        }

        // arraylist containing the totals set above, used to check against actual
        // arraylist
        ArrayList<Integer> totals = new ArrayList<Integer>(mcOptions.size());
        totals.add(25);
        totals.add(40);
        totals.add(10);
        totals.add(25);
        totals.add(25);

        assertEquals(question, mcr.getQuestion());
        assertEquals(mcOptions, mcr.getMultipleChoiceOptions());
        assertEquals(totals, mcr.getMultipleChoiceAnswers());
        assertEquals(5, mcr.getNumberOfOptions());
        assertEquals(qova.enums.ResponseType.MULTIPLE_CHOICE, mcr.getType());
        assertEquals(surveyPosition, mcr.getSurveyPosition());
        assertEquals(isDefaultQuestion, mcr.getIsDefaultQuestion());
    }

}
