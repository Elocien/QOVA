package qova.forms;

import java.util.ArrayList;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONObject;

import qova.enums.CourseType;

public class InstanceTitleForm {

    // String Arrays containing titles

    private String instanceTitlesJson;

    public InstanceTitleForm(String instanceTitlesJson) {
        this.instanceTitlesJson = instanceTitlesJson;

    }

    public String getInstanceTitlesJson() {
        return instanceTitlesJson;
    }

    /**
     * The Method takes the JSON String generated in InstanceTitles.html and
     * converts it to a {@link org.json.JSONArray}. On position for each position 0
     * to 3, the corresponding titles for lecture, tutorial, seminar and practical,
     * respectively, can be found. The method iterates through, and uses the titles
     * to set them in the {@linkplain qova.logic.courseManagement}
     * 
     * @param courseType
     * @return The instance titles as a {@link java.util.List}
     */
    public List<String> getInstanceTitlesForType(CourseType courseType) {

        List<String> instanceTitles = new ArrayList<>();

        JSONObject titlesAsJsonObject = new JSONObject(instanceTitlesJson);

        switch (courseType) {
            case LECTURE:
                JSONArray lectureTitlesAsArray = titlesAsJsonObject.getJSONArray("lecture");
                for (int i = 0; i < lectureTitlesAsArray.length(); i++) {
                    instanceTitles.add(lectureTitlesAsArray.getString(i));
                }
                break;

            case TUTORIAL:
                JSONArray tutorialTitlesAsArray = titlesAsJsonObject.getJSONArray("tutorial");
                for (int i = 0; i < tutorialTitlesAsArray.length(); i++) {
                    instanceTitles.add(tutorialTitlesAsArray.getString(i));
                }
                break;

            case SEMINAR:
                JSONArray seminarTitlesAsArray = titlesAsJsonObject.getJSONArray("seminar");
                for (int i = 0; i < seminarTitlesAsArray.length(); i++) {
                    instanceTitles.add(seminarTitlesAsArray.getString(i));
                }
                break;

            case PRACTICAL:
                JSONArray practicalTitlesAsArray = titlesAsJsonObject.getJSONArray("practical");
                for (int i = 0; i < practicalTitlesAsArray.length(); i++) {
                    instanceTitles.add(practicalTitlesAsArray.getString(i));
                }
                break;

            default:
                break;
        }

        return instanceTitles;
    }

}
