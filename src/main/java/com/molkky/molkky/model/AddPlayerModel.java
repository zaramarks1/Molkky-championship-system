package com.molkky.molkky.model;

import Type.UserRole;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.User;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class AddPlayerModel {

    public String surname;
    public String forename;
    public String club;
    public String mail;
    public Integer teamId;

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
        user.setRole(UserRole.PLAYER);
        user.setIsRegistered(false);
        user.setCode(createCode(10));

        return user;
    }

    public String createCode(int n){
        String AlphaNumericString = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                + "0123456789"
                + "abcdefghijklmnopqrstuvxyz";

        // create StringBuffer size of AlphaNumericString
        StringBuilder sb = new StringBuilder(n);

        for (int i = 0; i < n; i++) {

            // generate a random number between
            // 0 to AlphaNumericString variable length
            int index
                    = (int)(AlphaNumericString.length()
                    * Math.random());

            // add Character one by one in end of sb
            sb.append(AlphaNumericString
                    .charAt(index));
        }

        return sb.toString();

    }

}
