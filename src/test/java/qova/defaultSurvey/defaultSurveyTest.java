package qova.defaultSurvey;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import qova.AbstractIntegrationTest;
import qova.admin.DefaultSurvey;
import qova.enums.CourseType;

public class defaultSurveyTest extends AbstractIntegrationTest {

    @Test
    public void defaultSurveyConstructorTest() {

        var survey = "[{\"type\":\"OnetoFive\",\"question\":\"Hat die Vorlesung Wissen vermittelt, welches du dir nicht im Selbststudium hättest erarbeiten können?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Hat der/die Vorlesende den aktiven Austausch mit den Studierenden gesucht?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Waren die Anforderung dem Wissensstand der Studierenden angemessen?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Konnte die Vorlesung gezielt Schwerpunkte setzen und Struktur vermitteln?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Konnte der/die Vorlesende dein Interesse an dem Thema wecken?\",\"default\": true},{\"type\":\"SingleChoice\",\"question\":\"Online Lehre v.s. Präsenzveranstaltung\",\"answers\":[\"Die Vorlesung war digital und soll digital bleiben.\",\"Die Vorlesung war digital und wäre als Präsenzveranstaltung besser.\",\"Die Vorlesung war eine Präsenzveranstaltung und soll eine bleiben.\",\"Die Vorlesung war eine Präsenzveranstaltung und sollte digital werden.\"],\"default\": true},{\"type\":\"FreeText\",\"question\":\"An dieser Stelle würden wir uns über konstruktive Kritik, aber auch über Anregungen und Lob freuen!\"}]";

        DefaultSurvey defaultSurvey = new DefaultSurvey(CourseType.LECTURE);

        assertEquals(survey, defaultSurvey.getDefaultSurveyJson());
        assertEquals(CourseType.LECTURE, defaultSurvey.getCourseType());
    }

}
