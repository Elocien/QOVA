package qova.course;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Test;

import qova.AbstractIntegrationTest;
import qova.enums.CourseType;
import qova.objects.CourseInstance;

public class courseInstanceTest extends AbstractIntegrationTest {

    @Test
    public void courseInstanceConstructorTest() {

        var courseType = CourseType.SEMINAR;
        var groupAmount = 10;
        var instanceAmount = 12;
        var active = true;

        List<String> instanceTitles = new ArrayList<>();
        instanceTitles.addAll(
                Arrays.asList("Einführung", "Bitübertragungsschicht", "Netztechnologien 1", "Netztechnologien 2",
                        "Sicherungsschicht", "Vermittlungsschicht", "Transportschicht", "Netzwerkperformance",
                        "Internetdienste", "Multimediakommunikation", "Mobile Computing", "Verteilte Systeme"));

        CourseInstance courseInstance = new CourseInstance(courseType, groupAmount, instanceAmount, instanceTitles,
                active);

        assertEquals(courseType, courseInstance.getCourseType());
        assertEquals("[]", courseInstance.getSurvey());
        assertEquals(groupAmount, courseInstance.getGroupAmount());
        assertEquals(instanceAmount, courseInstance.getInstanceAmount());
        assertEquals(active, courseInstance.isActive());
        assertEquals(instanceTitles, courseInstance.getInstanceTitles());
        assertEquals(false, courseInstance.getSurveyEditedFlag());
    }
}
