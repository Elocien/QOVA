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
import java.util.Objects;
import java.util.Optional;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;


import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.data.category.DefaultCategoryDataset;

 

public class PDFGenerator {
    


    
    public void createPdf(ArrayList<Response> allResponses) throws IOException, NullPointerException {
        
        //Variables
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
        for(int pos = 0; pos < 100; pos++){

            //Get ResponseType for Responses of given position (pos). We assume this to be the same for every Response of that position (if error occurs, check serialisation of Responses)
            ResponseType responseType = responses.get(pos).get(0).getResponseType();

            //ArrayList of responses for the current Position. These will be from the same Couse and have the same: CourseType, classNo, ResponseType and position
            ArrayList<Response> responsesForPos = responses.get(pos);


            //Bar chart is created, if ResponseType was either Multiple_Choice or Drop_Down
            if(responseType == ResponseType.MULTIPLE_CHOICE || responseType == ResponseType.DROP_DOWN){

                for(int i = 0; i < responsesForPos.size(); i++){

                    //how to get values for column keys? Response object has arrayList of string, with each position corresponding to response

                    //Column values are accumulation of true statements for that column 
                    //Row key is 


                    //Chart generation
                    int width = 800;
                    int height = 600;

                    DefaultCategoryDataset dataSet = new DefaultCategoryDataset();
                    dataSet.setValue(791, "Population", "1750 AD");
                    dataSet.setValue(978, "Population", "1800 AD");
                    dataSet.setValue(1262, "Population", "1850 AD");
                    dataSet.setValue(1650, "Population", "1900 AD");
                    dataSet.setValue(2519, "Population", "1950 AD");
                    dataSet.setValue(6070, "Population", "2000 AD");

                    JFreeChart chart = ChartFactory.createBarChart("World Population growth", // title
                            "Year", // x-axis heading
                            "Population in millions", // y-axis heading
                            dataSet, // dataset
                            PlotOrientation.VERTICAL, // orientation
                            false, // Show legend
                            true, // Use Tooltips
                            false // Configure chart to generate URL's
                    );


                    ByteArrayOutputStream pngOutputStream = new ByteArrayOutputStream();
                    try {
                        ChartUtilities.writeChartAsPNG(pngOutputStream, chart, width, height);
                    } catch (IOException e) {
                        e.printStackTrace();
                    }

                    byte[] pngData = pngOutputStream.toByteArray(); 

                }

            }
            if(responseType == ResponseType.MULTIPLE_CHOICE || responseType == ResponseType.DROP_DOWN){

            }
            else if(responseType == ResponseType.TEXT_RESPONSE){

            }

            else if(responseType == ResponseType.BINARY_ANSWER){

            }

            else{
                throw new NullPointerException();
            }
        }










        //Chart generation
        int width = 800;
        int height = 600;

        DefaultCategoryDataset dataSet = new DefaultCategoryDataset();
        dataSet.setValue(791, "Population", "1750 AD");
        dataSet.setValue(978, "Population", "1800 AD");
        dataSet.setValue(1262, "Population", "1850 AD");
        dataSet.setValue(1650, "Population", "1900 AD");
        dataSet.setValue(2519, "Population", "1950 AD");
        dataSet.setValue(6070, "Population", "2000 AD");

        JFreeChart chart = ChartFactory.createBarChart("World Population growth", // title
                "Year", // x-axis heading
                "Population in millions", // y-axis heading
                dataSet, // dataset
                PlotOrientation.VERTICAL, // orientation
                false, // Show legend
                true, // Use Tooltips
                false // Configure chart to generate URL's
        );


        ByteArrayOutputStream pngOutputStream = new ByteArrayOutputStream();
        try {
            ChartUtilities.writeChartAsPNG(pngOutputStream, chart, width, height);
        } catch (IOException e) {
            e.printStackTrace();
        }

        byte[] pngData = pngOutputStream.toByteArray(); 


















        //For each elment of array of graphs:
            //add graph to document

        
        try {

            String dest = "src/main/resources/test.pdf";


            //Test iteration through arraylist
            Map<String, String> a = new HashMap<>();
            a.put("1", "Some response");
            a.put("2", "this is a text response");
            

            

            //Initialize PDF writer
            PdfWriter writer = new PdfWriter(dest);
    
            //Initialize PDF document
            PdfDocument pdf = new PdfDocument(writer);
            
            // Initialize document
            Document document = new Document(pdf);
    


            // Create a PdfFont
            PdfFont font = PdfFontFactory.createFont(StandardFonts.TIMES_ROMAN);
            // Add a Paragraph
            document.add(new Paragraph("iText is:").setFont(font));
            // Create a List
            List list = new List()
                .setSymbolIndent(12)
                .setListSymbol("\u2022")
                .setFont(font);




            //TODO
            for (String i : a.values()) {
                document.add(new Paragraph(i));
            }		



            ImageData data = ImageDataFactory.create(pngData);
            Image img = new Image(data); 
            document.add(img);

            // Add the list
            document.add(list);


            
            document.add(img);
    
            //Close document
            document.close();
        
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
    
    

}