package qova.forms;

import java.util.ArrayList;
import java.util.List;

import org.json.JSONArray;

import qova.enums.CourseType;

public class InstanceTitleForm {

    // String Arrays containing titles

    private String instanceTitlesJson;

    public InstanceTitleForm(String instanceTitlesJson) {
        this.instanceTitlesJson = instanceTitlesJson;
    }

    public List<String> getInstanceTitlesForType(CourseType courseType) {

        List<String> instanceTitles = new ArrayList<>();

        // JSONArray titlesAsJson = new JSONArray(instanceTitlesJson);

        // Parse the json and return instanceTitles as List<String>

        return instanceTitles;
    }

}

// [{"lecture" : ["1. lets go", "2. test title", "3. KEKW"]},{"tutorial":[]},
// {"seminar":[]}, {"practical":[]}]