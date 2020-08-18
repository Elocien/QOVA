package qova.responseLogic;

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

import qova.course.LocalizationOption;
import qova.responseTypes.BinaryResponse;
import qova.responseTypes.MultipleChoiceResponse;
import qova.responseTypes.ResponseType;
import qova.responseTypes.SingleChoiceResponse;
import qova.responseTypes.SurveyResponse;
import qova.responseTypes.TextResponse;


 

public final class PDFGenerator{

    /**
     * Generates PDF Documents based on the given SurveyResponse object. 
     * 
     * @param response {@linkplain SurveyResponse} 
     * @param language Defines the language of the resulting PDF
     * @return byte[]
     * @throws IOException
     * @throws Exception
     */
    public byte[] createPdf(SurveyResponse response, LocalizationOption language) throws IOException, Exception {

        //Localization variables
        String total;
        String responseOptions;
        String totalResponses;
        String totalYes;
        String totalNo;
        String multipleChoiceAndSingleChoiceResponsesTitle;
        String textResponsesTitle;
        String binaryResponsesTitle;

        if(language.equals(LocalizationOption.EN)){
            total = "Total";
            responseOptions = "Response Options";
            totalResponses = "Total Responses: ";
            totalYes = "Total 'Yes': ";
            totalNo = "Total 'No': ";
            multipleChoiceAndSingleChoiceResponsesTitle = "Multiple Choice and Single Choice Responses";
            textResponsesTitle = "Text Responses";
            binaryResponsesTitle = "Yes/No Responses";
        }
        else if(language.equals(LocalizationOption.DE)){
            total = "Stimmen Anzahl";
            responseOptions = "Antwortmöglichkeiten";
            totalResponses = "Gesamtstimmenanzahl: ";
            totalYes = "Gesamt 'Ja': ";
            totalNo = "Gesamt 'Nein': ";
            multipleChoiceAndSingleChoiceResponsesTitle = "Multiple Choice und Single Choice Fragen";
            textResponsesTitle = "Freitext Fragen";
            binaryResponsesTitle = "Ja/Nein Fragen";
        }


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
        

        //ArrayList containing all user responses.
        
        //Recap:
        //The SurveyResponse represents the questionnaire itself. Each of the objects returned by the getUserResponses() method
        //is 
        ArrayList<Object> rsp = response.getUserResponses();

        //Step 2:
        //Iterate through all user responses, with each object corresponding to a question on the questionnaire
        for(int i = 0; i < rsp.size(); i++){

            //Get the ResponseType of the next Object in the ArrayList 
            ResponseType currentResponseType;
            if(rsp.get(i) instanceof qova.responseTypes.BinaryResponse){
                currentResponseType = ResponseType.BINARY_ANSWER;
            }
            else if(rsp.get(i) instanceof qova.responseTypes.TextResponse){
                currentResponseType = ResponseType.TEXT_RESPONSE;
            }
            else if(rsp.get(i) instanceof qova.responseTypes.SingleChoiceResponse){
                currentResponseType = ResponseType.SINGLE_CHOICE;
            }
            else if(rsp.get(i) instanceof qova.responseTypes.MultipleChoiceResponse){
                currentResponseType = ResponseType.MULTIPLE_CHOICE;
            }
            else break;
            
                        
            //------------------------------------------------------------------------
            //This is used to determine what graphic to add to the PDF 
            //MULTIPLE_CHOICE or DROP_DOWN -- Bar Graph
            //BINARY_ANSWER -- Simple Text with % values for yes/no
            //TEXT_RESPONSE -- Table of TextResponses
            


            //----------------------------------------------------------------------------------------------------------------
            //Bar chart is created, if ResponseType is Multiple_Choice 
            if(currentResponseType.equals(ResponseType.MULTIPLE_CHOICE)){

                MultipleChoiceResponse mcr = (MultipleChoiceResponse) rsp.get(i);


                //The number of response possibilities determines the number of columns in the bar graph
                Integer responsePossibilities = mcr.getNumberOfOptions();

                //ArrayList containing all column titles 
                ArrayList<String> columnTitles = mcr.getMutltipleChoiceOptions();

                //DIFFERENT FROM columnTITLES!!!
                //Create an ArrayList, where each element represents a column of the bar graph. The value is the total number of responses for that option
                ArrayList<Integer> columnTotals = mcr.getMutltipleChoiceAnswers();


                //Initialise the DataSet
                DefaultCategoryDataset dataSet = new DefaultCategoryDataset();

                //Iterate through the columnDataList and add the data to the dataSet
                for (int j = 0; j < responsePossibilities; j++) {
                    dataSet.setValue(columnTotals.get(j), total, columnTitles.get(j));
                }


                //Chart Configuration
                JFreeChart chart = ChartFactory.createBarChart(
                        mcr.getQuestion(), // Title of BarGraph = Question in Survey
                        responseOptions, // x-axis heading
                        total, // y-axis heading
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



                        //----------------------------------------------------------------------------------------------------------------
            //Bar chart is created, if ResponseType was either Multiple_Choice or Drop_Down
            if(currentResponseType.equals(ResponseType.SINGLE_CHOICE)){

                SingleChoiceResponse scr = (SingleChoiceResponse) rsp.get(i);


                //The number of response possibilities determines the number of columns in the bar graph
                Integer responsePossibilities = scr.getNumberOfOptions();

                //ArrayList containing all column titles 
                ArrayList<String> columnTitles = scr.getMutltipleChoiceOptions();

                //DIFFERENT FROM columnTITLES!!!
                //Create an ArrayList, where each element represents a column of the bar graph. The value is the total number of responses for that option
                ArrayList<Integer> columnTotals = scr.getMutltipleChoiceAnswers();


                //Initialise the DataSet
                DefaultCategoryDataset dataSet = new DefaultCategoryDataset();

                //Iterate through the columnDataList and add the data to the dataSet
                for (int j = 0; j < responsePossibilities; j++) {
                    dataSet.setValue(columnTotals.get(j), total, columnTitles.get(j));
                }


                //Chart Configuration
                JFreeChart chart = ChartFactory.createBarChart(
                        scr.getQuestion(), // Title of BarGraph = Question in Survey
                        responseOptions, // x-axis heading
                        total, // y-axis heading
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

            else if(currentResponseType.equals(ResponseType.TEXT_RESPONSE)){

                TextResponse txr = (TextResponse) rsp.get(i);

                //Add Corresponding question to questionList
                Paragraph questionpara = new Paragraph();
                questionpara.setTextAlignment(TextAlignment.CENTER);
                questionpara.add(new Text(txr.getQuestion()).addStyle(header_2));
                TableQuestionList.add(questionpara);


                //Create Table
                Table table = new Table(UnitValue.createPercentArray(1)).useAllAvailableWidth();

                //Iterate through all responses at this position and add the textResponse to the table
                for (String r: txr.getResponses()){
                    table.addCell(r);
                }

                TableList.add(table);

            }
            //----------------------------------------------------------------------------------------------------------------






            //Create Box with Totals for all BinaryAnswers
            //----------------------------------------------------------------------------------------------------------------
            else if(currentResponseType.equals(ResponseType.BINARY_ANSWER)){

                BinaryResponse bnr = (BinaryResponse) rsp.get(i);

                //Create new Paragraph for Question
                Paragraph questionpara = new Paragraph();
                questionpara.setTextAlignment(TextAlignment.CENTER);
                questionpara.add(new Text(bnr.getQuestion()).addStyle(header_2));
                ParagraphList.add(questionpara);

                
                //Create new Paragraph for Results
                Paragraph para = new Paragraph();
                
                //Format Paragraph
                para.setTextAlignment(TextAlignment.CENTER);
                para.setBorder(new SolidBorder(1));

                
                //Count percentages of yes/no responses
                double yes = 0;
                double no = 0;

                //More variables for totals and percentages
                int tot = bnr.getNoTotal() + bnr.getYesTotal();
                String yesPercent = String.format("%.2f", (bnr.getYesTotal()/tot)*100) + "%";
                String noPercent = String.format("%.2f", (bnr.getNoTotal()/tot)*100) + "%";
                
            //add Text
                //Total    
                para.add(totalResponses);
                para.add(new Text(String.valueOf(total) + "\n").setFont(bold)); 

                //Yes Percentage
                para.add(totalYes);
                para.add(new Text(yesPercent + "\n").setFont(bold)); 

                //Yes Percentage
                para.add(totalNo);
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
        title.add(new Text(response.getCourse().getName()).addStyle(titleFont));
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
        document.add(new Paragraph(multipleChoiceAndSingleChoiceResponsesTitle).addStyle(header));
        document.add(lsDashed);


        //Bar charts containing multiple choice and drop down responses
        for(Image img: ImageList){
            document.add(img);
            document.add(new AreaBreak(AreaBreakType.NEXT_AREA));
        }


        //Tables containing text responses

        //Add title for questions of this type and a dashed line underneath
        document.add(new Paragraph(textResponsesTitle).addStyle(header));
        document.add(lsDashed);


        for(int i = 0; i < TableList.size(); i++){
            document.add(TableQuestionList.get(i));
            document.add(TableList.get(i));
            document.add(new AreaBreak(AreaBreakType.NEXT_AREA));
        }

        

        //Paragraph containing binary answer responses

        //Add title for questions of this type and a dashed line underneath
        document.add(new Paragraph(binaryResponsesTitle).addStyle(header));
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