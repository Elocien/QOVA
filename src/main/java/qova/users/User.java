package qova.users;

import org.hibernate.annotations.GenericGenerator;
import org.springframework.security.core.userdetails.UserDetails;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import java.util.UUID;

@Entity
public class User {

    @Id
    @GeneratedValue(generator = "UUID")
    @GenericGenerator(name = "UUID", strategy = "org.hibernate.id.UUIDGenerator")
    @Column(name = "id", updatable = false, nullable = false, columnDefinition = "BINARY(16)")
    private UUID id;

    // The Id given by Shibboleth
    private String ajpPersistentId;

    private String userRole;

    /**
     * Needed for JPA purposes
     */
    @SuppressWarnings("unused")
    protected User() {
    }


    public User(String ajpPersistentId, String userRole) {
        this.ajpPersistentId = ajpPersistentId;
        this.userRole = userRole;
    }

    public UUID getId() {
        return id;
    }

    public String getAjpPersistentId() {
        return ajpPersistentId;
    }

    public String getUserRole() {
        return this.userRole;
    }

    public UserDetails toCurrentUserDetails() {
        return CurrentUserDetails.create(this);
    }
}
