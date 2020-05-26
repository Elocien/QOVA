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
 
/**
 * Simple Hello World example.
 */
public class PDFGenerator {

 
    public void createPdf(String dest) throws IOException {
        
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



        //PDF Generation
        
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