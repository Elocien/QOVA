package qova.responses;

import java.io.ByteArrayOutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import com.opencsv.CSVWriter;

public class CSVGenerator {
    
    public byte[] createCSV(ArrayList<Response> allResponses) throws java.io.IOException {
        
           
        //Output Stream
        var stream = new ByteArrayOutputStream();

        //Output Stream Writer
        var outpuStreamWriter = new OutputStreamWriter(stream);

        // CSVWriter, OutputStreamWriter object as parameter 
        CSVWriter writer = new CSVWriter(outpuStreamWriter);




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
























        // CSV Standard Header (The options for all question types must be added after)
        String[] header = { "Course Name", "Instance", "Semster of Students", "Course Type", " Submission Date and Time", "Question", "Response Type", "Class No.", "Text Response", "Binary Answer" }; 
        writer.writeNext(header); 

        // add data to csv 
        String[] data1 = { "Aman", "10", "620" }; 
        writer.writeNext(data1); 
        String[] data2 = { "Suraj", "10", "630" }; 
        writer.writeNext(data2); 

        // closing writer connection 
        writer.close(); 
        


        return stream.toByteArray();
    }

    
}