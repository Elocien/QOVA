package qova.admin;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Lob;

import qova.enums.CourseType;

@Entity
public class DefaultSurvey {

    //Attributes
    @Id 
    private Long id;
    @Lob
    private String defaultSurveyJson;


    //Needed for JPA purposes
    @SuppressWarnings("unused")
	protected DefaultSurvey() {
    }


    //Constructor
    public DefaultSurvey(Long id, CourseType type){
        
        this.id = id;
        
        switch (type) {
            case LECTURE:
                this.defaultSurveyJson = "[{\"type\":\"SingleChoice\",\"question\":\"Hat die Vorlesung Wissen vermittelt, welches du dir nicht im Selbststudium hättest erarbeiten können?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Hat der/die Vorlesende den aktiven Austausch mit den Studierenden gesucht?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Waren die Anforderung dem Wissensstand der Studierenden angemessen?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Konnte die Vorlesung gezielt Schwerpunkte setzen und Struktur vermitteln?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Konnte der/die Vorlesende dein Interesse an dem Thema wecken?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Online Lehre v.s. Präsenzveranstaltung\",\"answers\":[\"Die Vorlesung war digital und soll digital bleiben.\",\"Die Vorlesung war digital und wäre als Präsenzveranstaltung besser.\",\"Die Vorlesung war eine Präsenzveranstaltung und soll eine bleiben.\",\"Die Vorlesung war eine Präsenzveranstaltung und sollte digital werden.\"]}]";
                break;
            case TUTORIAL:
                this.defaultSurveyJson = "[{\"type\":\"SingleChoice\",\"question\":\"Hat die Übung Wissen vermittelt, welches du dir nicht im Selbststudium hättest erarbeiten können?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Hat der/die Leiter/in den aktiven Austausch mit den Studierenden gesucht?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Waren die Anforderung dem Wissensstand der Studierenden angemessen?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Konnte die Übung gezielt Schwerpunkte setzen und Struktur vermitteln?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Konnte der/die Leiter/in dein Interesse an dem Thema wecken?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Hat der/die Leiter/in die Möglichkeiten einer Übung gegenüber der Vorlesung ausgeschöpft?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Online Lehre v.s. Präsenzveranstaltung\",\"answers\":[\"Die Übung war digital und soll digital bleiben.\",\"Die Übung war digital und wäre als Präsenzveranstaltung besser.\",\"Die Übung war eine Präsenzveranstaltung und soll eine bleiben.\",\"Die Übung war eine Präsenzveranstaltung und sollte digital werden.\"]}]";
                break;
            case SEMINAR:
                this.defaultSurveyJson = "[{\"type\":\"SingleChoice\",\"question\":\"Hat das Seminar Wissen vermittelt, welches du dir nicht im Selbststudium hättest erarbeiten können?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Hat der/die Leiter/in den aktiven Austausch mit den Studierenden gesucht?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Waren die Anforderung dem Wissensstand der Studierenden angemessen?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Konnte das Seminar gezielt Schwerpunkte setzen und Struktur vermitteln?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Konnte der/die Leiter/in dein Interesse an dem Thema wecken?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Hat der/die Leiter/in die Möglichkeiten eines Seminars gegenüber der Vorlesung ausgeschöpft?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Online Lehre v.s. Präsenzveranstaltung\",\"answers\":[\"Das Seminar war digital und soll digital bleiben.\",\"Das Seminar war digital und wäre als Präsenzveranstaltung besser.\",\"Das Seminar war eine Präsenzveranstaltung und soll eine bleiben.\",\"Das Seminar war eine Präsenzveranstaltung und sollte digital werden.\"]}]";
                break;
            case PRACTICAL:
                this.defaultSurveyJson = "[{\"type\":\"SingleChoice\",\"question\":\"Hat das Praktikum Wissen vermittelt, welches du dir nicht im Selbststudium hättest erarbeiten können?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Hat der/die Leiter/in den aktiven Austausch mit den Studierenden gesucht?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Waren die Anforderung dem Wissensstand der Studierenden angemessen?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Konnte der/die Leiter/in dein Interesse an dem Thema wecken?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Hat der/die Leiter/in die Möglichkeiten eines Praktikums gegenüber eines Seminares ausgeschöpft?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Konntest du durch die praktischen Anwendungen Kompetenz aufbauen/festigen?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"]},{\"type\":\"SingleChoice\",\"question\":\"Online Lehre v.s. Präsenzveranstaltung\",\"answers\":[\"Das Praktikum war digital und soll digital bleiben.\",\"Das Praktikum war digital und wäre als Präsenzveranstaltung besser.\",\"Das Praktikum war eine Präsenzveranstaltung und soll eine bleiben.\",\"Das Praktikum war eine Präsenzveranstaltung und sollte digital werden.\"]}]";
                break;
            default:
                break;
        }
    }


    //Getters and Setters
    public Long getId(){
        return this.id;
    }

    public void setDefaultSurveyJson(String surveyJson){
        this.defaultSurveyJson = surveyJson;
    }

    public String getDefaultSurveyJson(){
        return this.defaultSurveyJson;
    }
}