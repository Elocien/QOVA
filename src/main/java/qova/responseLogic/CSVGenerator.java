package qova.responseLogic;

import java.io.ByteArrayOutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Arrays;

import com.opencsv.CSVWriter;

import qova.course.Course;
import qova.course.LocalizationOption;
import qova.responseTypes.UserResponse;

public class CSVGenerator {
    
    /**
     * Generates CSV of student responses
     * 
     * @param allResponses
     * @param language
     * @return
     * @throws java.io.IOException
     */
    public byte[] createCSV(ArrayList<UserResponse> allResponses, LocalizationOption language) throws java.io.IOException {
        
        //Initialise Data Structures

        //Response Attributes
        //We use the Response at position 0, because it has the same Course and CourseType as all other Responses given. See ResponseManagement for details on the retrieval of Responses for generation
        UserResponse response = allResponses.get(0);
        String courseType = String.valueOf(response.getCourseType());
        String classNo = String.valueOf(response.getClassNo());
        

        //Course Specific Attributes
        Course course = response.getCourse();
        String courseName = course.getName();
        String courseInstanceSemesterForm = course.getSemesterString();
        String semesterOfStudents = String.valueOf(course.getSemesterOfStudents());
        
        //CSV Header
        ArrayList<String> header = new ArrayList<String>();

        // CSV Header Initialisation (The options for all question types must be added after)
        if(language.equals(LocalizationOption.EN)){
            header.addAll(Arrays.asList("Course Name", "Instance", "Semster of Students", "Course Type", "Class No.", "Survey Questions->"));
        }
        else if(language.equals(LocalizationOption.DE)){
            header.addAll(Arrays.asList("Lehrveranstaltungsname", "Instanz", "Fachsemester der Studenten", "Lehrveranstaltungstyp", "Gruppen Nr.", "Fragebogen Fragen ->"));
        }
             
        // CSV Date 
        ArrayList<String> data = new ArrayList<String>( Arrays.asList(courseName, courseInstanceSemesterForm, semesterOfStudents, courseType, classNo));





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