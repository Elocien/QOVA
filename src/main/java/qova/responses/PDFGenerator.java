package qova.responses;

import com.itextpdf.io.font.constants.StandardFonts;
import com.itextpdf.io.image.ImageData;
import com.itextpdf.io.image.ImageDataFactory;
import com.itextpdf.kernel.font.PdfFont;
import com.itextpdf.kernel.font.PdfFontFactory;
import com.itextpdf.kernel.pdf.PdfDocument;
import com.itextpdf.kernel.pdf.PdfWriter;
import com.itextpdf.layout.Document;
import com.itextpdf.layout.element.Image;
import com.itextpdf.layout.element.List;
import com.itextpdf.layout.element.Paragraph;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;


import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.data.category.DefaultCategoryDataset;

 

public final class PDFGenerator {
    


    
    public byte[] createPdf(ArrayList<Response> allResponses) throws IOException, Exception {
        
        //Variables
        //List containing all graphs
        ArrayList<Image> ImageList = new ArrayList<Image>();


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
        //Iterate through map
        for(int pos = 0; pos < responses.size(); pos++){

            

            //ArrayList of responses for the current Position. These will be from the same Course and have the same: CourseType, classNo, ResponseType and position
            ArrayList<Response> responsesForPos = responses.get(pos);




            //Get ResponseType for Responses of given position (pos). We assume this to be the same for every Response of that position (if error occurs, check serialisation of Responses)
            //------------------------------------------------------------------------
            //This is used to determine what graphic to add to the PDF 
            //MULTIPLE_CHOICE or DROP_DOWN -- Bar Graph
            //BINARY_ANSWER -- Simple Text with % values for yes/no
            //TEXT_RESPONSE -- Table of TextResponses
            ResponseType responseType = responsesForPos.get(0).getResponseType();


            //Bar chart is created, if ResponseType was either Multiple_Choice or Drop_Down
            if(responseType.equals(ResponseType.MULTIPLE_CHOICE) || responseType.equals(ResponseType.DROP_DOWN)){


                //The number of response possibilities determines the number of columns in the bar graph
                Integer responsePossibilities = responsesForPos.get(0).getResponsePossibilities();

                //ArrayList containing all column titles 
                ArrayList<String> columnTitles = responsesForPos.get(0).getOptionsMCDD();

                //Create an ArrayList, where each element represents a column of the bar graph. The value is the total number of responses for that option
                ArrayList<Integer> columnTotals = new ArrayList<Integer>(responsePossibilities);

                //Populate the list
                for (int j = 0; j < responsePossibilities; j++) {
                    columnTotals.add(0);
                }


                //Main-Loop (Iterate through all Responses)
                //Accumulate the values for each column (using columnDataList)
                for (Response r: responsesForPos){

                    // TODO: Check if this causes performance issues. If so, do random checks outside of loop
                    // checks to make sure response matches those at the same position

                    //check responseType
                    if(r.getResponseType() != responseType){
                        throw new Exception("ResponseType does not match that of others at this position, error in Serialisation");
                    }
                    //check OptionsMCDD
                    else if(!(r.getOptionsMCDD().equals(columnTitles))){
                        throw new Exception("optionsMCDD does not match that of others at this position, error in Serialisation");
                    }
                    //check responsePossibilities
                    else if(r.getListMCDD().size() != responsePossibilities){
                        throw new Exception("Length of list of MC or DD responses (ListMCDD) does not match that of others at this position, error in Serialisation");
                    }


                    //iterate through values from list of user responses 
                    for(int j = 0; j < r.getListMCDD().size(); j++){

                        //if true --> increment column value
                        if(r.getListMCDD().get(j) == true){
                            int columnTotal = columnTotals.get(j);
                            columnTotals.set(j, columnTotal + 1);
                        }
                    }
                }


                //Initialise the DataSet
                DefaultCategoryDataset dataSet = new DefaultCategoryDataset();

                //Iterate through the columnDataList and add the data to the dataSet
                for (int j = 0; j < responsePossibilities; j++) {
                    dataSet.setValue(columnTotals.get(j), "total", columnTitles.get(j));
                }
                

                //Chart Configuration
                JFreeChart chart = ChartFactory.createBarChart(
                        responsesForPos.get(0).getQuestion(), // Title of BarGraph = Question in Survey
                        "Response", // x-axis heading
                        "Total", // y-axis heading
                        dataSet, // dataset
                        PlotOrientation.VERTICAL, // orientation
                        false, // Show legend
                        true, // Use Tooltips
                        false // Configure chart to generate URL's
                );

                //Chart Dimensions
                int width = 600;
                int height = 600;





                //Create OutputStream of the graph
                ByteArrayOutputStream pngOutputStream = new ByteArrayOutputStream();
                try {
                    ChartUtilities.writeChartAsPNG(pngOutputStream, chart, width, height);
                } catch (IOException e) {
                    e.printStackTrace();
                }

                //Add Graph to PDF
                ImageData data = ImageDataFactory.create(pngOutputStream.toByteArray());
                Image img = new Image(data); 

                ImageList.add(img);


            }
            else if(responseType == ResponseType.TEXT_RESPONSE){

            }

            else if(responseType == ResponseType.BINARY_ANSWER){

            }

            else{
                throw new Exception("");
            }
        }






        //Create PDF Document

        var stream = new ByteArrayOutputStream();
        var writer = new PdfWriter(stream);
        var pdf = new PdfDocument(writer);
        var document = new Document(pdf);


        for (Image img: ImageList){
            document.add(img);
        }

        
        //Close document
        document.close();


        
        

        //Return PDF as byte[]
        return stream.toByteArray();


    }
}