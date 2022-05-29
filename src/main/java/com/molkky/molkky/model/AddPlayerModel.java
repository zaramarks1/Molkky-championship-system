package com.molkky.molkky.model;


import com.molkky.molkky.domain.User;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang.RandomStringUtils;

@Getter
@Setter
public class AddPlayerModel {

    private String surname;
    private String forename;
    private String club;
    private String mail;
    private Integer teamId;

    public AddPlayerModel() {
    }

    public AddPlayerModel(String surname, String forename, String club, String mail) {
        this.surname = surname;
        this.forename = forename;
        this.club = club;
        this.mail = mail;
    }

    public AddPlayerModel(String surname, String forename, String club, String mail, Integer teamId) {
        this.surname = surname;
        this.forename = forename;
        this.club = club;
        this.mail = mail;
        this.teamId = teamId;
    }

    public User addPlayer(){
        User user = new User();

        user.setSurname(this.getSurname());
        user.setForename(this.getForename());
        user.setEmail(this.getMail());
        user.setClub(this.getClub());


        return user;
    }

    public String createCode(int n){
        return RandomStringUtils.randomAlphabetic(n);
    }

}
