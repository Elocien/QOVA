package qova.users;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class CurrentUserDetails implements UserDetails {

    private final String ROLE_PREFIX = "ROLE_";
    private final String ajpPersistentId;
    private final UserRole userRole;


    public CurrentUserDetails(String ajpPersistentId, UserRole userRole){
        this.ajpPersistentId = ajpPersistentId;
        this.userRole = userRole;
    }

    public static UserDetails create(User user) {
        return new CurrentUserDetails(user.getAjpPersistentId(), user.getUserRole());
    }

    @Override
    public Collection<? extends GrantedAuthority> getAuthorities() {
        List<GrantedAuthority> list = new ArrayList<>();
        list.add(new SimpleGrantedAuthority(ROLE_PREFIX + userRole));
        return list;
    }

    @Override
    public String getPassword() {
        return null;
    }

    @Override
    public String getUsername() {
        return this.ajpPersistentId;
    }

    @Override
    public boolean isAccountNonExpired() {
        return false;
    }

    @Override
    public boolean isAccountNonLocked() {
        return false;
    }

    @Override
    public boolean isCredentialsNonExpired() {
        return false;
    }

    @Override
    public boolean isEnabled() {
        return false;
    }
}