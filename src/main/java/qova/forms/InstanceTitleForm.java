package qova.forms;

import java.util.ArrayList;
import java.util.List;

import qova.enums.CourseType;

public class InstanceTitleForm {

    // String Arrays containing titles

    private String instanceTitlesJson;

    public InstanceTitleForm(String instanceTitlesJson) {
        this.instanceTitlesJson = instanceTitlesJson;
    }

    public List<String> getInstanceTitlesForType(CourseType courseType) {

        List<String> instanceTitles = new ArrayList<>();

        // Parse the json and return instanceTitles as List<String>

        return instanceTitles;
    }

}