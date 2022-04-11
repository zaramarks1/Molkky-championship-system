package com.molkky.molkky.domain;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.util.Set;

@Getter
@Entity
@Setter
@Table(name = "user")
public class User {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "pseudo")
    private String pseudo;

    @Column(name = "surname")
    private String surname;

    @Column(name = "forename")
    private String forename;

    @Column(name = "club")
    private String club;

    @Column(name = "email")
    private String email;

    @Column(name = "isRegistered")
    private Boolean isRegistered;

    @Column(name = "role")
    private String role;

    @Column(name = "code")
    String code;


    @OneToMany
    @JoinColumn(name="idUser", nullable = true)
    private Set<Notification> notifications;

    @ManyToOne
    @JoinColumn(name="idTournement", nullable = true)
    private Tournament tournament;

    @ManyToOne
    @JoinColumn(name="idTeam", nullable = true)
    private Team team;

    public User(String pseudo, String surname, String forename, String club, String email, Boolean isRegistered, String role) {

        this.pseudo = pseudo;
        this.surname = surname;
        this.forename = forename;
        this.club = club;
        this.email = email;
        this.isRegistered = isRegistered;
        this.role = role;

    }

    public User(Integer id, String pseudo, String surname, String forename, String club, String email, Boolean isRegistered, String role, String code, Tournament tournament) {
        this.id = id;
        this.pseudo = pseudo;
        this.surname = surname;
        this.forename = forename;
        this.club = club;
        this.email = email;
        this.isRegistered = isRegistered;
        this.role = role;
        this.code = code;
        this.tournament = tournament;
    }

    public User(String email, String code){
        this.email=email;
        this.code=code;
    }
    public User() {

    }
}
