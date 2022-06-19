package com.molkky.molkky.model;


import com.molkky.molkky.domain.Club;
import com.molkky.molkky.domain.User;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang.RandomStringUtils;

@Getter
@Setter
public class AddPlayerModel {

    private String surname;
    private String forename;
    private String pseudo;
    private Club club;
    private String mail;
    private Integer teamId;

    public AddPlayerModel() {
    }

    public User addPlayer(){
        User user = new User();

        user.setSurname(this.getSurname());
        user.setForename(this.getForename());
        user.setEmail(this.getMail());
        user.setClub(this.getClub());
        user.setPseudo(this.getPseudo());

        return user;
    }

    public String createCode(int n){
        return RandomStringUtils.randomAlphabetic(n);
    }

}
