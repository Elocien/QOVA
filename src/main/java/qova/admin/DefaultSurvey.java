package qova.admin;

import java.util.UUID;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Lob;

import org.hibernate.annotations.GenericGenerator;

import qova.enums.CourseType;

@Entity
public class DefaultSurvey {

    // Attributes
    @Id
    @GeneratedValue(generator = "UUID")
    @GenericGenerator(name = "UUID", strategy = "org.hibernate.id.UUIDGenerator")
    @Column(name = "id", updatable = false, nullable = false, columnDefinition = "BINARY(16)")
    private UUID id;

    @Lob
    private String defaultSurveyJson;

    @Enumerated
    private CourseType courseType;

    // Needed for JPA purposes
    @SuppressWarnings("unused")
    protected DefaultSurvey() {
    }

    // Constructor
    public DefaultSurvey(CourseType type) {

        this.courseType = type;

        switch (type) {
            case LECTURE:
                this.defaultSurveyJson = "[{\"type\":\"OnetoFive\",\"question\":\"Hat die Vorlesung Wissen vermittelt, welches du dir nicht im Selbststudium hättest erarbeiten können?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Hat der/die Vorlesende den aktiven Austausch mit den Studierenden gesucht?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Waren die Anforderung dem Wissensstand der Studierenden angemessen?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Konnte die Vorlesung gezielt Schwerpunkte setzen und Struktur vermitteln?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Konnte der/die Vorlesende dein Interesse an dem Thema wecken?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Online Lehre v.s. Präsenzveranstaltung\",\"answers\":[\"Die Vorlesung war digital und soll digital bleiben.\",\"Die Vorlesung war digital und wäre als Präsenzveranstaltung besser.\",\"Die Vorlesung war eine Präsenzveranstaltung und soll eine bleiben.\",\"Die Vorlesung war eine Präsenzveranstaltung und sollte digital werden.\"],\"default\": true},{\"type\":\"FreeText\",\"question\":\"An dieser Stelle würden wir uns über konstruktive Kritik, aber auch über Anregungen und Lob freuen!\"}]";
                break;
            case TUTORIAL:
                this.defaultSurveyJson = "[{\"type\":\"OnetoFive\",\"question\":\"Hat die Übung Wissen vermittelt, welches du dir nicht im Selbststudium hättest erarbeiten können?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Hat der/die Leiter/in den aktiven Austausch mit den Studierenden gesucht?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Waren die Anforderung dem Wissensstand der Studierenden angemessen?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Konnte die Übung gezielt Schwerpunkte setzen und Struktur vermitteln?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Konnte der/die Leiter/in dein Interesse an dem Thema wecken?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Hat der/die Leiter/in die Möglichkeiten einer Übung gegenüber der Vorlesung ausgeschöpft?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Online Lehre v.s. Präsenzveranstaltung\",\"answers\":[\"Die Übung war digital und soll digital bleiben.\",\"Die Übung war digital und wäre als Präsenzveranstaltung besser.\",\"Die Übung war eine Präsenzveranstaltung und soll eine bleiben.\",\"Die Übung war eine Präsenzveranstaltung und sollte digital werden.\"],\"default\": true},{\"type\":\"FreeText\",\"question\":\"An dieser Stelle würden wir uns über konstruktive Kritik, aber auch über Anregungen und Lob freuen!\"}]";
                break;
            case SEMINAR:
                this.defaultSurveyJson = "[{\"type\":\"OnetoFive\",\"question\":\"Hat das Seminar Wissen vermittelt, welches du dir nicht im Selbststudium hättest erarbeiten können?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Hat der/die Leiter/in den aktiven Austausch mit den Studierenden gesucht?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Waren die Anforderung dem Wissensstand der Studierenden angemessen?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Konnte das Seminar gezielt Schwerpunkte setzen und Struktur vermitteln?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Konnte der/die Leiter/in dein Interesse an dem Thema wecken?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Hat der/die Leiter/in die Möglichkeiten eines Seminars gegenüber der Vorlesung ausgeschöpft?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Online Lehre v.s. Präsenzveranstaltung\",\"answers\":[\"Das Seminar war digital und soll digital bleiben.\",\"Das Seminar war digital und wäre als Präsenzveranstaltung besser.\",\"Das Seminar war eine Präsenzveranstaltung und soll eine bleiben.\",\"Das Seminar war eine Präsenzveranstaltung und sollte digital werden.\"],\"default\": true},{\"type\":\"FreeText\",\"question\":\"An dieser Stelle würden wir uns über konstruktive Kritik, aber auch über Anregungen und Lob freuen!\"}]";
                break;
            case PRACTICAL:
                this.defaultSurveyJson = "[{\"type\":\"OnetoFive\",\"question\":\"Hat das Praktikum Wissen vermittelt, welches du dir nicht im Selbststudium hättest erarbeiten können?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Hat der/die Leiter/in den aktiven Austausch mit den Studierenden gesucht?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Waren die Anforderung dem Wissensstand der Studierenden angemessen?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Konnte der/die Leiter/in dein Interesse an dem Thema wecken?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Hat der/die Leiter/in die Möglichkeiten eines Praktikums gegenüber eines Seminares ausgeschöpft?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Konntest du durch die praktischen Anwendungen Kompetenz aufbauen/festigen?\",\"default\": true},{\"type\":\"OnetoFive\",\"question\":\"Online Lehre v.s. Präsenzveranstaltung\",\"answers\":[\"Das Praktikum war digital und soll digital bleiben.\",\"Das Praktikum war digital und wäre als Präsenzveranstaltung besser.\",\"Das Praktikum war eine Präsenzveranstaltung und soll eine bleiben.\",\"Das Praktikum war eine Präsenzveranstaltung und sollte digital werden.\"],\"default\": true},{\"type\":\"FreeText\",\"question\":\"An dieser Stelle würden wir uns über konstruktive Kritik, aber auch über Anregungen und Lob freuen!\"}]";
                break;
            default:
                break;
        }
    }

    // Getters and Setters
    public UUID getId() {
        return this.id;
    }

    public CourseType getCourseType() {
        return this.courseType;
    }

    public void setDefaultSurveyJson(String surveyJson) {
        this.defaultSurveyJson = surveyJson;
    }

    public String getDefaultSurveyJson() {
        return this.defaultSurveyJson;
    }
}