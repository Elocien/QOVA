package qova.logic;

import java.io.ByteArrayOutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Arrays;

import com.opencsv.CSVWriter;

import qova.enums.LocalizationOption;
import qova.objects.Course;
import qova.objects.SurveyResponse;


public class CSVGenerator {
    
    /**
     * Generates a CSV of the Responses for a single SurveyResponse
     * 
     * @param response {@linkplain SurveyResponse} 
     * @param language {@linkplain LocalizationOption}
     * @return a byte[] of the CSV
     * @throws java.io.IOException Thrown by CSVWriter
     */
    public byte[] createCSV(SurveyResponse response, LocalizationOption language) throws java.io.IOException {
        
        //Initialise Data Structures

        //Response Attributes
        //We use the Response at position 0, because it has the same Course and CourseType as all other Responses given. See ResponseManagement for details on the retrieval of Responses for generation
        String courseType = String.valueOf(response.getCourseType());
        String groupNumber = String.valueOf(response.getGroupNumber());
        String instanceNumber = String.valueOf(response.getInstanceNumber());
        

        //Course Specific Attributes
        Course course = response.getCourse();
        String courseName = course.getName();
        String courseInstanceAsSemester = course.getSemesterString();
        String semesterOfStudents = String.valueOf(course.getSemesterOfStudents());
        
        
        //CSV Header
        ArrayList<String> header = new ArrayList<String>();

        // CSV Header Initialisation (The options for all question types must be added after)
        if(language.equals(LocalizationOption.EN)){
            header.addAll(Arrays.asList("Course Name", "Instance", "Semster of Students", "Course Type", "Tutorial Group", "Instance", "Survey Questions->"));
        }
        else if(language.equals(LocalizationOption.DE)){
            header.addAll(Arrays.asList("Lehrveranstaltungsname", "Instanz", "Fachsemester der Studenten", "Lehrveranstaltungstyp", "Übungsgruppe", "Instanz", "Fragebogen Fragen ->"));
        }
             
        // CSV Data
        ArrayList<String> data = new ArrayList<String>( Arrays.asList(courseName, courseInstanceAsSemester, semesterOfStudents, courseType, groupNumber, instanceNumber));





        //Append Header with the rest of the survey questions







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
       
        //Write Data
        String[] dataArray = data.toArray(new String[0]);
        writer.writeNext(dataArray); 
        
        // closing writer connection 
        writer.close(); 
        
        //CSV as byte[] stream
        return stream.toByteArray();
    }
    
}