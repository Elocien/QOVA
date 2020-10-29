package qova;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.core.userdetails.AuthenticationUserDetailsService;
import org.springframework.security.core.userdetails.UserDetailsByNameServiceWrapper;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.web.authentication.preauth.PreAuthenticatedAuthenticationProvider;
import org.springframework.security.web.authentication.preauth.PreAuthenticatedAuthenticationToken;
import org.springframework.security.web.authentication.preauth.RequestHeaderAuthenticationFilter;
import qova.users.CustomRequestAuthenticationFilter;


@Configuration
@EnableWebSecurity
public class SecurityConfiguration extends WebSecurityConfigurerAdapter {


    /**
     * The custom defined implementation of {@linkplain UserDetailsService}
     */
    @Autowired
    private qova.users.UserDetailsService userDetailsService;


    /**
     * Configures The global Authentication, i.e. how does authentication take place.
     *
     * @param auth The {@linkplain AuthenticationManagerBuilder} which has the {@link #customAuthenticationProvider()} ()} as
     *             its {@linkplain org.springframework.security.authentication.AuthenticationProvider} implementation
     */
    @Autowired
    public void configureGlobal(AuthenticationManagerBuilder auth)  {
        auth.authenticationProvider(customAuthenticationProvider());
    }

    @Bean
    public PreAuthenticatedAuthenticationProvider customAuthenticationProvider(){

        AuthenticationUserDetailsService<PreAuthenticatedAuthenticationToken> wrapper =
                new UserDetailsByNameServiceWrapper<>(userDetailsService);

        PreAuthenticatedAuthenticationProvider preAuthenticatedAuthenticationProvider = new PreAuthenticatedAuthenticationProvider();
        preAuthenticatedAuthenticationProvider.setPreAuthenticatedUserDetailsService(wrapper);

        return preAuthenticatedAuthenticationProvider;
    }


    /**
     * The {@linkplain org.springframework.security.web.authentication.AuthenticationFilter} which takes the Shibboleth
     * AJP headers
     *
     * @return The custom {@linkplain RequestHeaderAuthenticationFilter}
     * @throws Exception From the {@linkplain org.springframework.security.authentication.AuthenticationManager}
     */
    @Bean
    protected CustomRequestAuthenticationFilter shibAuthenticationFilter() throws Exception {
        CustomRequestAuthenticationFilter requestHeaderAuthenticationFilter = new CustomRequestAuthenticationFilter();
        requestHeaderAuthenticationFilter.setAuthenticationManager(authenticationManagerBean());
        return  requestHeaderAuthenticationFilter;
    }




    @Override
    protected void configure(HttpSecurity http) throws Exception {
        http
                .addFilter(shibAuthenticationFilter())
                .authorizeRequests()
                .antMatchers("/course/**").hasRole("STAFF")
                .anyRequest().permitAll()
                .and().logout().logoutSuccessUrl("/");
    }


}