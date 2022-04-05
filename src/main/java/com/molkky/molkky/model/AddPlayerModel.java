package com.molkky.molkky.model;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class AddPlayerModel {

    public String surname;
    public String forename;
    public String club;
    public String mail;

    public AddPlayerModel() {
    }

    public AddPlayerModel(String surname, String forename, String club, String mail) {
        this.surname = surname;
        this.forename = forename;
        this.club = club;
        this.mail = mail;
    }
}
