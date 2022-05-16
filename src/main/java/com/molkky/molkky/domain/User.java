package com.molkky.molkky.domain;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.LazyCollection;
import org.hibernate.annotations.LazyCollectionOption;


import javax.persistence.*;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;


@Getter
@Entity
@Setter
@Table(name = "user")
@AllArgsConstructor
@NoArgsConstructor
public class User implements Serializable {
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

    @Column(name = "password")
    private String password;


    @OneToMany(mappedBy = "user")
    private List<Notification> notifications = new ArrayList<>();

    @OneToMany()
    @LazyCollection(LazyCollectionOption.FALSE)
    @JoinColumn(name="idUser", nullable = true)
    private List<UserTournamentRole> userTournamentRoles = new ArrayList<>();


    public User(String pseudo, String surname, String forename, String club, String email) {
        this.pseudo = pseudo;
        this.surname = surname;
        this.forename = forename;
        this.club = club;
        this.email = email;

    }
}
