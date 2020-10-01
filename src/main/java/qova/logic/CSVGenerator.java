package qova.logic;

import java.io.ByteArrayOutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import com.opencsv.CSVWriter;

import qova.enums.LocalizationOption;
import qova.objects.AbstractResponse;
import qova.objects.BinaryResponse;
import qova.objects.Course;
import qova.objects.MultipleChoiceResponse;
import qova.objects.SingleChoiceResponse;
import qova.objects.SurveyResponse;

public class CSVGenerator {

    /**
     * Generates a CSV of the Responses for a single SurveyResponse. The method can
     * only take SurveyResponses that belong to the same
     * {@linkplain qova.objects.Course} and of the same
     * {@linkplain qova.enums.CourseType}. Each line of the CSV represents one
     * {@linkplain qova.objects.SurveyResponse} object.
     * 
     * @param listOfSurveyResponse List of {@linkplain SurveyResponse}s
     * @param language             {@linkplain LocalizationOption}
     * @return a byte[] of the CSV
     * @throws java.io.IOException Thrown by CSVWriter
     */
    public byte[] createCSV(List<SurveyResponse> listOfSurveyResponses, LocalizationOption language)
            throws java.io.IOException {

        // Standard values, which are identical for every one of the SurveyResponses
        // passed to the CSV Gen
        // It is assumed that all SurveyResponses passed to the CSV Gen are of the same
        // Course and CourseType
        Course course = listOfSurveyResponses.get(0).getCourse();
        String courseName = course.getName();
        String courseSemesterString = course.getSemesterString();
        String semesterOfStudents = String.valueOf(course.getSemesterOfStudents());

        // CSV Header
        ArrayList<String> header = new ArrayList<>();

        // Second Header
        ArrayList<String> underHeader = new ArrayList<>();

        // CSV Header Initialisation (The options for all question types must be added
        // after)
        if (language.equals(LocalizationOption.EN)) {
            header.addAll(Arrays.asList("Name", "Semester Period", "Semster of Students", "Group", "Instance"));

        } else if (language.equals(LocalizationOption.DE)) {
            header.addAll(Arrays.asList("Lehrveranstaltungsname", "Findet Statt", "Fachsemester", "Gruppe", "Instanz"));
        }

        // Fill the underhead with blanks for the above header attributes
        underHeader.addAll(Arrays.asList("", "", "", "", ""));

        // Append Header with the rest of the survey questions
        for (AbstractResponse abstractResponse : listOfSurveyResponses.get(0).getListOfResponses()) {

            // Adds the question to the header, and a blank field to the underheader
            if (abstractResponse instanceof qova.objects.BinaryResponse) {
                header.add(((BinaryResponse) abstractResponse).getQuestion());
                header.add("");
                underHeader.add("Total Yes");
                underHeader.add("Total No");
            }

            // //Adds the question to the header, and a blank field to the underheader
            // if(o instanceof qova.objects.TextResponse){
            // header.add(((TextResponse) o).getQuestion());
            // underHeader.add("");
            // }

            // Adds the question to the header, and a blank field to the underheader. Then,
            // for each singleChoiceOption, a blank field is added to the header and the
            // option itself to the underheader
            if (abstractResponse instanceof qova.objects.SingleChoiceResponse) {
                SingleChoiceResponse scr = (SingleChoiceResponse) abstractResponse;
                header.add(scr.getQuestion());
                underHeader.add(scr.getSingleChoiceOptions().get(0));
                for (int i = 1; i < scr.getNumberOfOptions(); i++) {
                    header.add("-");
                    underHeader.add(scr.getSingleChoiceOptions().get(i));
                }
            }

            // Adds the question to the header, and a blank field to the underheader. Then,
            // for each multipleChoiceOption, a blank field is added to the header and the
            // option itself to the underheader
            if (abstractResponse instanceof qova.objects.MultipleChoiceResponse) {
                MultipleChoiceResponse mcr = (MultipleChoiceResponse) abstractResponse;
                header.add(mcr.getQuestion());
                underHeader.add(mcr.getMultipleChoiceOptions().get(0));
                for (int i = 1; i < mcr.getNumberOfOptions(); i++) {
                    header.add("-");
                    underHeader.add(mcr.getMultipleChoiceOptions().get(i));
                }
            }
        }

        List<List<String>> listOfCSVRowData = new ArrayList<>();

        for (SurveyResponse currentSurveyResponse : listOfSurveyResponses) {

            // CSV Standar Values
            List<String> row = new ArrayList<>(Arrays.asList(courseName, courseSemesterString, semesterOfStudents));

            row.addAll(
                    Arrays.asList(String.valueOf(currentSurveyResponse.getGroupNumber()), String.valueOf(currentSurveyResponse.getInstanceNumber());


            // Append Header with the rest of the survey questions
            for (AbstractResponse abstractResponse : currentSurveyResponse.getListOfResponses()) {

                // Adds the yes and no totals to the row
                if (abstractResponse instanceof qova.objects.BinaryResponse) {
                    row.add((((BinaryResponse) abstractResponse).getYesTotalString()));
                    row.add((((BinaryResponse) abstractResponse).getNoTotalString()));
                }

                // if(o instanceof qova.objects.TextResponse){
                // Text Responses are not shown in the csv
                // }

                // Adds the question to the header, and a blank field to the underheader. Then,
                // for each singleChoiceOption, a blank field is added to the header and the
                // option itself to the underheader
                if (abstractResponse instanceof qova.objects.SingleChoiceResponse) {
                    for (Integer total : (((SingleChoiceResponse) abstractResponse).getSingleChoiceAnswers())) {
                        row.add(String.valueOf(total));
                    }
                }

                // Adds the question to the header, and a blank field to the underheader. Then,
                // for each multipleChoiceOption, a blank field is added to the header and the
                // option itself to the underheader
                if (abstractResponse instanceof qova.objects.MultipleChoiceResponse) {
                    for (Integer total : (((MultipleChoiceResponse) abstractResponse).getMultipleChoiceAnswers())) {
                        row.add(String.valueOf(total));
                    }
                }
            }
            listOfCSVRowData.add(row);
        }

        // setup CSVWriter and Stream
        // ----------------------------------------------------------------------------------------------------------

        // Output Stream
        var stream = new ByteArrayOutputStream();

        // Output Stream Writer
        var outpuStreamWriter = new OutputStreamWriter(stream);

        // CSVWriter, OutputStreamWriter object as parameter
        CSVWriter writer = new CSVWriter(outpuStreamWriter);

        // ----------------------------------------------------------------------------------------------------------

        // Write Header
        String[] headerArray = header.toArray(new String[0]);
        writer.writeNext(headerArray);

        String[] underheaderArray = underHeader.toArray(new String[0]);
        writer.writeNext(underheaderArray);

        for (List<String> data : listOfCSVRowData) {
            // Write Data
            String[] dataArray = data.toArray(new String[0]);
            writer.writeNext(dataArray);
        }

        // closing writer connection
        writer.close();

        // CSV as byte[] stream
        return stream.toByteArray();
    }

}