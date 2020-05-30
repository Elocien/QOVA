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

import org.springframework.beans.factory.annotation.Autowired;

import qova.course.CourseType;

import qova.course.Course;

 

public class PDFGenerator {
    
    @Autowired
    private final ResponseRepository responseRepository;
    
    @Autowired
    PDFGenerator(ResponseRepository responseRepository){
        this.responseRepository = Objects.requireNonNull(responseRepository);
    }
    


    /**
     * This class generates the PDF with the results of a survey. It takes all response objects that correspond to a survey and generates the following, based on {@linkplain ResponseType}:
     * MULTIPLE_CHOICE, DROP_DOWN           - Bar Graph
     * TEXT_RESPONSE                        - List of text responses
     * BINARY_ANSWER                        - TODO: decide what to generate
     * 
     * @param dest                          Where the file is saved (TODO: dont save file, pass it to {@linkplain ResponseController} as byte[])
     * @param course                        {@linkplain Course} which is used to fetch the corresponding {@linkplain Response} objects
     * @throws IOException                  Throws runntime exception, in case of IOException when generating PDF
     */
    public void createPdf(String dest, Course course, CourseType courseType, Integer classNo) throws IOException, NullPointerException {
        
        //Variables
        //Map that contains responses, ordered by position
        Map<Integer, ArrayList<Response>> responses = new HashMap<>();









        //Class has multiple steps

        //Step 1:
            //Get all responses for the given course
            //add to Map

        //Step 2:
            //For each position in map:
                //Get ResponseType: (either mult-choice, dropdown, textresponse or binaryanswer):
                    //generate appropriate graphic and add to pdf

        //Step 3:
            //Return PDF









            



        //Step 1:

        //Get responses, based on position, up to the maximum of 100 (this is the maximum possible amount of responses)
        for(int pos = 1; pos < 100; pos++){
            
            //Container for responses
            ArrayList<Response> currentPosResponses = new ArrayList<Response>();

            //Limit for the number of consecutively emtpy responses tolerated
            Integer limit = 5;

            //Iterator that is the temporary container for all Responses of the position being iterated over
            Iterable<Response> respIterator = responseRepository.findByCourseAndCourseTypeAndClassNoAndPosition(course, courseType, classNo, pos);

            //Add responses to arrayList
            for(Response rsp: respIterator){
                currentPosResponses.add(rsp);
            }

            //Add arrayList of all responses for current position to map, with the key being the position
            responses.put(pos, currentPosResponses);

            //Logic: If there are no responses for 5 consecutive positions, break
            if(currentPosResponses.size() == 0){
                limit -= 1;
                if(limit == 0){
                    break;
                }
            }
            else{
                limit = 5;
            }
   
        }  
        
        

    


















        //Step 2:
        //Iterate through map
        for(int pos = 0; pos < 100; pos++){

            //Get ResponseType for Responses of given position (pos). We assume this to be the same for every Response of that position (if error occurs, check serialisation of Responses)
            ResponseType responseType = responses.get(pos).get(0).getResponseType();



            //Bar chart is created, if ResponseType was either Multiple_Choice or Drop_Down
            if(responseType == ResponseType.MULTIPLE_CHOICE || responseType == ResponseType.DROP_DOWN){


        





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