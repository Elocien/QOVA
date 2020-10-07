package qova.defaultSurvey;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import qova.AbstractIntegrationTest;
import qova.admin.DefaultSurvey;
import qova.enums.CourseType;

public class defaultSurveyTest extends AbstractIntegrationTest {

    @Test
    public void defaultSurveyConstructorTest() {

        var survey = "[{\"type\":\"SingleChoice\",\"question\":\"Hat die Vorlesung Wissen vermittelt, welches du dir nicht im Selbststudium hättest erarbeiten können?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Hat der/die Vorlesende den aktiven Austausch mit den Studierenden gesucht?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Waren die Anforderung dem Wissensstand der Studierenden angemessen?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Konnte die Vorlesung gezielt Schwerpunkte setzen und Struktur vermitteln?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Konnte der/die Vorlesende dein Interesse an dem Thema wecken?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Online Lehre v.s. Präsenzveranstaltung\",\"answers\":[\"Die Vorlesung war digital und soll digital bleiben.\",\"Die Vorlesung war digital und wäre als Präsenzveranstaltung besser.\",\"Die Vorlesung war eine Präsenzveranstaltung und soll eine bleiben.\",\"Die Vorlesung war eine Präsenzveranstaltung und sollte digital werden.\"]}]";

        DefaultSurvey defaultSurvey = new DefaultSurvey(CourseType.LECTURE);

        assertEquals(survey, defaultSurvey.getDefaultSurveyJson());
        assertEquals(CourseType.LECTURE, defaultSurvey.getCourseType());
    }

}
