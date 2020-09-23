package qova.logic;

import java.io.ByteArrayOutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.opencsv.CSVWriter;

import qova.enums.LocalizationOption;
import qova.objects.BinaryResponse;
import qova.objects.Course;
import qova.objects.MultipleChoiceResponse;
import qova.objects.SingleChoiceResponse;
import qova.objects.SurveyResponse;


public class CSVGenerator {
    
    /**
     * Generates a CSV of the Responses for a single SurveyResponse. The method can only take SurveyResponses that belong to the same {@linkplain qova.objects.Course} and of the same {@linkplain qova.enums.CourseType}.
     * Each line of the CSV represents one {@linkplain qova.objects.SurveyResponse} object. 
     * 
     * @param listOfSurveyResponse List of {@linkplain SurveyResponse}s 
     * @param language {@linkplain LocalizationOption}
     * @return a byte[] of the CSV
     * @throws java.io.IOException Thrown by CSVWriter
     */
    public byte[] createCSV(List<SurveyResponse> listOfSurveyResponses, LocalizationOption language) throws java.io.IOException {
        

        //Standard values, which are identical for every one of the SurveyResponses passed to the CSV Gen
        //It is assumed that all SurveyResponses passed to the CSV Gen are of the same Course and CourseType
        SurveyResponse response = listOfSurveyResponses.get(0);
        Course course = listOfSurveyResponses.get(0).getCourse();
        String courseName = course.getName();
        String courseSemesterString = course.getSemesterString();
        String semesterOfStudents = String.valueOf(course.getSemesterOfStudents());
        String courseType = String.valueOf(response.getCourseType());


        //CSV Header
        ArrayList<String> header = new ArrayList<>();

        //Second Header
        ArrayList<String> underHeader = new ArrayList<>();

        // CSV Header Initialisation (The options for all question types must be added after)
        if(language.equals(LocalizationOption.EN)){
            header.addAll(Arrays.asList("Name", "","Semster of Students", "Course Type", "Group", "Instance", "Survey Questions->"));
            
        }
        else if(language.equals(LocalizationOption.DE)){
            header.addAll(Arrays.asList("Lehrveranstaltungsname", "Findet Statt", "Fachsemester", "Lehrveranstaltungstyp", "Gruppe", "Instanz", "Fragebogen Fragen ->"));
        }

        //Fill the underhead with blanks for the above header attributes
        underHeader.addAll(Arrays.asList("", "", "", "", "", "", ""));

        
        //Append Header with the rest of the survey questions
        for(Object o : response.getUserResponses()){
            
            //Adds the question to the header, and a blank field to the underheader
            if(o instanceof qova.objects.BinaryResponse){
                header.add(((BinaryResponse) o).getQuestion());
                header.add("");
                underHeader.add("Total Yes");
                underHeader.add("Total No");
            }

            // //Adds the question to the header, and a blank field to the underheader
            // if(o instanceof qova.objects.TextResponse){
            //     header.add(((TextResponse) o).getQuestion());
            //     underHeader.add("");
            // }

            //Adds the question to the header, and a blank field to the underheader. Then, for each singleChoiceOption, a blank field is added to the header and the option itself to the underheader
            if(o instanceof qova.objects.SingleChoiceResponse){
                SingleChoiceResponse scr = (SingleChoiceResponse) o;
                header.add(scr.getQuestion());
                underHeader.add("");
                for(int i = 0; i < scr.getNumberOfOptions(); i++){
                    header.add("-");
                    underHeader.add(scr.getSingleChoiceOptions().get(i));
                }
            }

             //Adds the question to the header, and a blank field to the underheader. Then, for each multipleChoiceOption, a blank field is added to the header and the option itself to the underheader
            if(o instanceof qova.objects.MultipleChoiceResponse){
                MultipleChoiceResponse mcr = (MultipleChoiceResponse) o;
                header.add(mcr.getQuestion());
                underHeader.add("");
                for(int i = 0; i < mcr.getNumberOfOptions(); i++){
                    header.add("-");
                    underHeader.add(mcr.getMultipleChoiceOptions().get(i));
                }
            }
        }
             
        


        List<List<String>> listOfCSVRowData = new ArrayList<>();

        for(SurveyResponse s : listOfSurveyResponses){
            // CSV Standar Values
            List<String> row = new ArrayList<>( Arrays.asList(courseName, courseSemesterString, semesterOfStudents, courseType, String.valueOf(s.getGroupNumber()), String.valueOf(s.getInstanceNumber()), ""));

            //Append Header with the rest of the survey questions
            for(Object o : s.getUserResponses()){
                
                //Adds the yes and no totals to the row
                if(o instanceof qova.objects.BinaryResponse){
                    row.add((((BinaryResponse) o).getYesTotalString()));
                    row.add((((BinaryResponse) o).getNoTotalString()));
                }

                
                // if(o instanceof qova.objects.TextResponse){
                    //Text Responses are not shown in the csv
                // }

                //Adds the question to the header, and a blank field to the underheader. Then, for each singleChoiceOption, a blank field is added to the header and the option itself to the underheader
                if(o instanceof qova.objects.SingleChoiceResponse){
                    for (Integer total : (((SingleChoiceResponse) o).getSingleChoiceAnswers())) {
                        row.add(String.valueOf(total));
                    }
                }

                //Adds the question to the header, and a blank field to the underheader. Then, for each multipleChoiceOption, a blank field is added to the header and the option itself to the underheader
                if(o instanceof qova.objects.MultipleChoiceResponse){
                    for (Integer total : (((MultipleChoiceResponse) o).getMultipleChoiceAnswers())) {
                        row.add(String.valueOf(total));
                    }
                }
            }
            listOfCSVRowData.add(row); 
        }













        //setup CSVWriter and Stream
        //----------------------------------------------------------------------------------------------------------

        //Output Stream
        var stream = new ByteArrayOutputStream();

        //Output Stream Writer
        var outpuStreamWriter = new OutputStreamWriter(stream);

        // CSVWriter, OutputStreamWriter object as parameter 
        CSVWriter writer = new CSVWriter(outpuStreamWriter);


        //----------------------------------------------------------------------------------------------------------




        










        
        












        

        //Write Header
        String[] headerArray = header.toArray(new String[0]);
        writer.writeNext(headerArray); 

        String[] underheaderArray = underHeader.toArray(new String[0]);
        writer.writeNext(underheaderArray); 
       
        for(List<String> data : listOfCSVRowData){
            //Write Data
            String[] dataArray = data.toArray(new String[0]);
            writer.writeNext(dataArray); 
        }
        
        
        // closing writer connection 
        writer.close(); 
        
        //CSV as byte[] stream
        return stream.toByteArray();
    }
    
}