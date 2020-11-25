package qova;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.core.userdetails.*;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.authentication.preauth.PreAuthenticatedAuthenticationProvider;
import org.springframework.security.web.authentication.preauth.PreAuthenticatedAuthenticationToken;
import org.springframework.security.web.authentication.preauth.RequestHeaderAuthenticationFilter;
import qova.users.CustomRequestAuthenticationFilter;
import qova.users.UserRepository;


@Configuration
@EnableWebSecurity
public class SecurityConfiguration extends WebSecurityConfigurerAdapter {

    @Bean
    public PasswordEncoder encoder() {
        return new BCryptPasswordEncoder();
    }



    protected void configure(final AuthenticationManagerBuilder auth) throws Exception {
        auth.inMemoryAuthentication()
                .withUser("student").password(encoder().encode("student")).roles("STUDENT")
                .and()
                .withUser("staff").password(encoder().encode("staff")).roles("STAFF")
                .and()
                .withUser("admin").password(encoder().encode("admin")).roles("ADMIN");
    }

    @Override
    protected void configure(HttpSecurity http) throws Exception {
        http
                .authorizeRequests()
                .antMatchers("/course/**").hasAnyRole("STAFF", "ADMIN")
                .antMatchers("/admin/**").hasAnyRole("STAFF", "ADMIN")
                .antMatchers("/survey/**").hasAnyRole("STUDENT","STAFF", "ADMIN")
                .anyRequest().permitAll()
                .and().formLogin()
                .and().logout().clearAuthentication(true).deleteCookies("JSESSIONID").invalidateHttpSession(true)
                .logoutUrl("/logout").logoutSuccessUrl("/");
    }


}