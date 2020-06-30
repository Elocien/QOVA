package qova.responses;

import java.io.ByteArrayOutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import com.opencsv.CSVWriter;

import qova.course.Course;

public class CSVGenerator {
    
    public byte[] createCSV(ArrayList<Response> allResponses) throws java.io.IOException {
        
        //Initialise Data Structures

        //Response Attributes
        //We use the Response at position 0, because it has the same Course and CourseType as all other Responses given. See ResponseManagement for details on the retrieval of Responses for generation
        Response response = allResponses.get(0);
        String courseType = String.valueOf(response.getCourseType());
        String question = response.getQuestion();
        String classNo = String.valueOf(response.getClassNo());
        

        //Course Specific Attributes
        Course course = response.getCourse();
        String courseName = course.getName();
        String courseInstanceSemesterForm = course.getSemesterString();
        String semesterOfStudents = String.valueOf(course.getSemesterOfStudents());
        





        // CSV Header Initialisation (The options for all question types must be added after)
        ArrayList<String> header = new ArrayList<String>( Arrays.asList("Course Name", "Instance", "Semster of Students", "Course Type", "Question", "Class No."));
        
        // CSV Date 
        ArrayList<String> data = new ArrayList<String>( Arrays.asList( courseName, courseInstanceSemesterForm, semesterOfStudents, courseType, question, classNo));







        //setup CSVWriter and Stream
        //----------------------------------------------------------------------------------------------------------

        //Output Stream
        var stream = new ByteArrayOutputStream();

        //Output Stream Writer
        var outpuStreamWriter = new OutputStreamWriter(stream);

        // CSVWriter, OutputStreamWriter object as parameter 
        CSVWriter writer = new CSVWriter(outpuStreamWriter);


        //----------------------------------------------------------------------------------------------------------




        //Map that contains responses/ The key is the position of response objects in ArrayList
        Map<Integer, ArrayList<Response>> responses = new HashMap<Integer, ArrayList<Response>>();

        //Iterate through ArrayList and add each response to the correct ArrayList of the HashMap. The key of the 
        for(int i = 0; i < allResponses.size(); i++){
            Response rsp = allResponses.get(i);
            Integer pos = rsp.getPosition();
            
            //temporary List which holds the Responses for a given key in the map
            ArrayList<Response> tempList = responses.get(pos);

            // if list does not exist create it
            if(tempList == null) {
                tempList = new ArrayList<Response>();
                tempList.add(rsp);
                responses.put(pos, tempList);
            } else {
                // add if item is not already in list
                if(!tempList.contains(rsp)){
                    tempList.add(rsp);
                } 
            }
        }










        
        












        //Step 2:
        //Main Loop; iterate through all responses
        for(int pos = 0; pos < responses.size(); pos++){

            //ArrayList of responses for the current Position. These will be from the same Course and have the same: CourseType, classNo, ResponseType and position
            ArrayList<Response> responsesForPos = responses.get(pos);

            //Get ResponseType for Responses of given position (pos). We assume this to be the same for every Response of that position (if error occurs, check serialisation of Responses)
            ResponseType responseType = responsesForPos.get(0).getResponseType();


            //Multiple Choice or Single Choice
            if(responseType.equals(ResponseType.MULTIPLE_CHOICE) || responseType.equals(ResponseType.SINGLE_CHOICE)){
                
                //appends each of the multiple choice options to the header
                header.addAll(responsesForPos.get(0).getOptionsMCDD());

                //passes all responses at the current position to the method, which accumulates them and addes them to the data 
                for (Response r: responsesForPos){

                }
            }

            else if(responseType == ResponseType.TEXT_RESPONSE){
                //appends each of the multiple choice options to the header
                header.add(responsesForPos.get(0).getQuestion());
            }

            else if(responseType == ResponseType.BINARY_ANSWER){
                header.add(responsesForPos.get(0).getQuestion());
            }

            else{

            }
        }


        //Write Header
        String[] headerArray = header.toArray(new String[0]);
        writer.writeNext(headerArray); 
       

        //Write Data
        String[] dataArray = data.toArray(new String[0]);
        writer.writeNext(dataArray); 
        



        // closing writer connection 
        writer.close(); 
        


        return stream.toByteArray();
    }













    
    
}