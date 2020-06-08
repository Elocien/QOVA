package qova.responses;

import com.itextpdf.io.font.constants.StandardFonts;
import com.itextpdf.io.image.ImageData;
import com.itextpdf.io.image.ImageDataFactory;
import com.itextpdf.kernel.font.PdfFont;
import com.itextpdf.kernel.font.PdfFontFactory;
import com.itextpdf.kernel.pdf.PdfDocument;
import com.itextpdf.kernel.pdf.PdfWriter;
import com.itextpdf.kernel.pdf.canvas.draw.DashedLine;
import com.itextpdf.kernel.pdf.canvas.draw.SolidLine;
import com.itextpdf.layout.Document;
import com.itextpdf.layout.Style;
import com.itextpdf.layout.borders.SolidBorder;
import com.itextpdf.layout.element.AreaBreak;
import com.itextpdf.layout.element.Image;
import com.itextpdf.layout.element.LineSeparator;
import com.itextpdf.layout.element.Paragraph;
import com.itextpdf.layout.element.Table;
import com.itextpdf.layout.element.Text;
import com.itextpdf.layout.property.AreaBreakType;
import com.itextpdf.layout.property.TextAlignment;
import com.itextpdf.layout.property.UnitValue;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import java.io.ByteArrayOutputStream;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.data.category.DefaultCategoryDataset;

 

public final class PDFGenerator {

    public byte[] createPdf(ArrayList<Response> allResponses, String PdfTitle) throws IOException, Exception {
        
        //SETUP
        //----------------------------------------------------------------------------------------------------------------------

        //Variables
        //List of all BarGraphs (Multiple Choice and Drop Down)
        ArrayList<Image> ImageList = new ArrayList<Image>();

        //List of all Tables (TextResponse) and their corresponding titles
        ArrayList<Paragraph> TableQuestionList = new ArrayList<Paragraph>();
        ArrayList<Table> TableList = new ArrayList<Table>();
        

        //List of all Paragraphs (BinaryAnswer)
        ArrayList<Paragraph> ParagraphList = new ArrayList<Paragraph>();


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


        
        //Initialise PDF Document and create OutputStream, PdfWriter 
        var stream = new ByteArrayOutputStream();
        var writer = new PdfWriter(stream);
        var pdf = new PdfDocument(writer);
        var document = new Document(pdf);


        //Set Fonts (This has to be done within an instance of the PDF)
        PdfFont font = PdfFontFactory.createFont(StandardFonts.TIMES_ROMAN);
        PdfFont bold = PdfFontFactory.createFont(StandardFonts.TIMES_BOLD);
        
        Style titleFont = new Style();
        titleFont.setFont(bold).setFontSize(36);
        titleFont.setTextAlignment(TextAlignment.CENTER);

        Style header = new Style();
        header.setFont(bold).setFontSize(30);
        header.setTextAlignment(TextAlignment.CENTER);

        Style header_2 = new Style();
        header.setFont(font).setFontSize(18);
        header.setTextAlignment(TextAlignment.CENTER);
        
        
        //----------------------------------------------------------------------------------------------------------------------
        


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






            //----------------------------------------------------------------------------------------------------------------
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


                //Add
                ImageList.add(img);

            }
            //----------------------------------------------------------------------------------------------------------------






            //Create Table for all TextResponses
            //----------------------------------------------------------------------------------------------------------------

            else if(responseType == ResponseType.TEXT_RESPONSE){

                //Add Corresponding question to questionList
                Paragraph questionpara = new Paragraph();
                questionpara.setTextAlignment(TextAlignment.CENTER);
                questionpara.add(new Text(responsesForPos.get(0).getQuestion()).addStyle(header_2));
                TableQuestionList.add(questionpara);


                //Create Table
                Table table = new Table(UnitValue.createPercentArray(1)).useAllAvailableWidth();

                //Iterate through all responses at this position and add the textResponse to the table
                for (Response r: responsesForPos){
                    table.addCell(r.getTextResponse());
                }

                //Add table to List of Tables;
                TableList.add(table);

            }
            //----------------------------------------------------------------------------------------------------------------






            //Create Box with Totals for all BinaryAnswers
            //----------------------------------------------------------------------------------------------------------------
            else if(responseType == ResponseType.BINARY_ANSWER){

                //Create new Paragraph for Question
                Paragraph questionpara = new Paragraph();
                questionpara.setTextAlignment(TextAlignment.CENTER);
                questionpara.add(new Text(responsesForPos.get(0).getQuestion()).addStyle(header_2));
                ParagraphList.add(questionpara);

                
                //Create new Paragraph for Results
                Paragraph para = new Paragraph();
                
                //Format Paragraph
                para.setTextAlignment(TextAlignment.CENTER);
                para.setBorder(new SolidBorder(1));

                
                //Count percentages of yes/no responses
                double yes = 0;
                double no = 0;

                //Iterate through all responses to get totals
                for (Response r: responsesForPos){
                    

                    if(r.getBinaryAnswer().equals(false)){
                        no++;
                    }
                    else{
                        yes++;
                    }
                }

                //More variables for totals and percentages
                int total = (int)(yes+no); 
                String yesPercent = String.format("%.2f", (yes/total)*100) + "%";
                String noPercent = String.format("%.2f", (no/total)*100) + "%";
                
            //add Text
                //Total    
                para.add("Total Responses: ");
                para.add(new Text(String.valueOf(total) + "\n").setFont(bold)); 

                //Yes Percentage
                para.add("Total Yes: ");
                para.add(new Text(yesPercent + "\n").setFont(bold)); 

                //Yes Percentage
                para.add("Total No: ");
                para.add(new Text(noPercent).setFont(bold)); 

                //Add table to List of Tables;
                ParagraphList.add(para);

            }
            //----------------------------------------------------------------------------------------------------------------

            else{
                throw new Exception("ResponseType matched none of the required values");
            }
        }






        //Set the Title of the PDF
        //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Paragraph title = new Paragraph();

        //Title text is given as function argument 
        title.add(new Text(PdfTitle).addStyle(titleFont));
        title.setTextAlignment(TextAlignment.CENTER);

        //Line underneath title
        //title.addline

        //Bottom margin
        title.setMarginBottom(10);

        //Add title to pdf
        document.add(title);
        //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



        //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        //Add Line to seperate title from the rest of the document
        SolidLine line = new SolidLine(1f);
        
        LineSeparator ls = new LineSeparator(line);
        ls.setMarginTop(-5);
        ls.setMarginBottom(20);

        document.add(ls);

        //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


        //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        //Create dashed line separator 
        DashedLine dashedLine = new DashedLine(1f);
        LineSeparator lsDashed = new LineSeparator(dashedLine);
        lsDashed.setMarginBottom(10);
        //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




        //Tables containing text responses

        //Add title for questions of this type and a dashed line underneath
        document.add(new Paragraph("Mehrfachauswahl und Dropdown-Liste").addStyle(header));
        document.add(lsDashed);


        //Bar charts containing multiple choice and drop down responses
        for(Image img: ImageList){
            document.add(img);
            document.add(new AreaBreak(AreaBreakType.NEXT_AREA));
        }


        //Tables containing text responses

        //Add title for questions of this type and a dashed line underneath
        document.add(new Paragraph("Freitexte").addStyle(header));
        document.add(lsDashed);


        for(int i = 0; i < TableList.size(); i++){
            document.add(TableQuestionList.get(i));
            document.add(TableList.get(i));
            document.add(new AreaBreak(AreaBreakType.NEXT_AREA));
        }

        

        //Paragraph containing binary answer responses

        //Add title for questions of this type and a dashed line underneath
        document.add(new Paragraph("Ja/Nein Fragen").addStyle(header));
        document.add(lsDashed);

        //Iterate through the list, in jumps of 2, because there are pairs of paragraphs
        for(int i = 0; i < ParagraphList.size(); i = i + 2){
            
            //Add Title of Binary Answer (The question itself)
            document.add(ParagraphList.get(i));

            //Add Block with results
            document.add(ParagraphList.get(i+1));

            //Add whitespace
            document.add(new Paragraph("\n \n \n"));
        }
        






        //Close document
        document.close();

        //Return PDF as byte[]
        return stream.toByteArray();


    }
}