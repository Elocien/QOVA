package qova;

import lombok.Data;
import org.apache.catalina.connector.Connector;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.LocaleResolver;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;
import org.springframework.web.servlet.i18n.LocaleChangeInterceptor;
import org.springframework.web.servlet.i18n.SessionLocaleResolver;

@SpringBootApplication
public class QovaApplication implements WebMvcConfigurer {

	public static void main(String[] args) {
		SpringApplication.run(QovaApplication.class, args);
	}

	@Bean
	public LocaleResolver localeResolver() {
		return new SessionLocaleResolver();
	}

	@Bean
	public LocaleChangeInterceptor localeChangeInterceptor() {
		LocaleChangeInterceptor localeChangeInterceptor = new LocaleChangeInterceptor();
		localeChangeInterceptor.setParamName("lang");
		return localeChangeInterceptor;
	}

	@Override
	public void addInterceptors(InterceptorRegistry interceptorRegistry) {
		interceptorRegistry.addInterceptor(localeChangeInterceptor());
	}

	//Tomcat Config
	//-----------------------------------------
	@Configuration
	@Data
	public class TomcatConfiguration {

		@Value("${tomcat.ajp.port}")
		int ajpPort;

		@Value("${tomcat.ajp.remoteauthentication}")
		String remoteAuthentication;

		@Value("${tomcat.ajp.enabled}")
		boolean tomcatAjpEnabled;

		@Bean
		public TomcatServletWebServerFactory servletContainer() {

			TomcatServletWebServerFactory tomcat = new TomcatServletWebServerFactory();
			if (tomcatAjpEnabled)
			{
				Connector ajpConnector = new Connector("AJP/1.3");
				ajpConnector.setPort(ajpPort);
				ajpConnector.setSecure(false);
				ajpConnector.setAllowTrace(false);
				ajpConnector.setScheme("https");
				ajpConnector.setAttribute("tomcatAuthentication", remoteAuthentication);
				ajpConnector.setAttribute("packetSize", 65536);
				tomcat.addAdditionalTomcatConnectors(ajpConnector);
			}

			return tomcat;
		}

	}

}
