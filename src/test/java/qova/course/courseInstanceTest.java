package qova.course;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Test;

import org.mockito.Mockito;
import qova.AbstractIntegrationTest;
import qova.admin.DefaultSurvey;
import qova.enums.CourseType;
import qova.objects.CourseInstance;

public class courseInstanceTest extends AbstractIntegrationTest {

    @Test
    public void activeCourseInstanceConstructorTest() {

        DefaultSurvey defaultSurvey = Mockito.mock(DefaultSurvey.class);

        var courseType = CourseType.SEMINAR;
        var groupAmount = 10;
        var instanceAmount = 12;

        List<String> instanceTitles = new ArrayList<>(Arrays.asList("Einführung", "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2",
                "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance",
                "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"));

        CourseInstance courseInstance = new CourseInstance(courseType, groupAmount, instanceAmount, instanceTitles, defaultSurvey);

        assertEquals(courseType, courseInstance.getCourseType());
        assertEquals("[]", courseInstance.getSurvey());
        assertEquals(groupAmount, courseInstance.getGroupAmount());
        assertEquals(instanceAmount, courseInstance.getInstanceAmount());
        assertEquals(true, courseInstance.isActive());
        assertEquals(instanceTitles, courseInstance.getInstanceTitles());
        assertEquals(false, courseInstance.getSurveyEditedFlag());
        assertEquals(defaultSurvey, courseInstance.getDefaultSurvey());
    }

    @Test
    public void inactiveCourseInstanceConstructorTest() {

        DefaultSurvey defaultSurvey = Mockito.mock(DefaultSurvey.class);

        CourseInstance courseInstance = new CourseInstance(CourseType.TUTORIAL, defaultSurvey);

        assertEquals(CourseType.TUTORIAL, courseInstance.getCourseType());
        assertEquals("[]", courseInstance.getSurvey());
        assertEquals(0, courseInstance.getGroupAmount());
        assertEquals(0, courseInstance.getInstanceAmount());
        assertEquals(false, courseInstance.isActive());
        assertEquals(false, courseInstance.getSurveyEditedFlag());
        assertEquals(defaultSurvey, courseInstance.getDefaultSurvey());
    }

}
