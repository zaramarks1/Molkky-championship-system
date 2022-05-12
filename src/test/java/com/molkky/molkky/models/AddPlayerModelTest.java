package com.molkky.molkky.models;

import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.AddPlayerModel;
import com.molkky.molkky.model.AddPlayerlistModel;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class AddPlayerModelTest {

    @Test
    void testAddPlayerConstructor1(){
        AddPlayerModel player = new AddPlayerModel();
        player.setForename("Zara");
        player.setSurname("Marks");
        player.setMail("zara.marks@reseau.eseo.fr");
        player.setClub("Molkky Angers");

        Assertions.assertEquals("Marks",player.getSurname(), "Surname different");
        Assertions.assertEquals("Zara",player.getForename(), "Forename different");
        Assertions.assertEquals("Molkky Angers",player.getClub(), "Club different");
        Assertions.assertEquals("zara.marks@reseau.eseo.fr",player.getMail(), "Mail different");
    }

    @Test
    void testAddPlayerConstructor2(){
        AddPlayerModel player = new AddPlayerModel();
        player.setForename("Zara");
        player.setSurname("Marks");
        player.setMail("zara.marks@reseau.eseo.fr");
        player.setClub("Molkky Angers");
        player.setTeamId(1);

        Assertions.assertEquals(Integer.valueOf(1),player.getTeamId(),"Id club different");
    }

    @Test
    void testAddPlayerFunction(){
        AddPlayerModel player = new AddPlayerModel();
        player.setForename("Zara");
        player.setSurname("Marks");
        player.setMail("zara.marks@reseau.eseo.fr");
        player.setClub("Molkky Angers");
        User user = player.addPlayer();

        Assertions.assertEquals(player.getSurname(),user.getSurname(), "Surname different");
        Assertions.assertEquals(player.getForename(),user.getForename(),"Forname different");
        Assertions.assertEquals(player.getClub(),user.getClub(),"Club different");
        Assertions.assertEquals(player.getMail(),user.getEmail(),"Mail different");
    }

    @Test
    void testCreateCodeLength(){
        AddPlayerModel player = new AddPlayerModel();
        player.setForename("Zara");
        player.setSurname("Marks");
        player.setMail("zara.marks@reseau.eseo.fr");
        player.setClub("Molkky Angers");

        String code = player.createCode(10);
        Assertions.assertEquals(10,code.length(), "Length not good");
    }

    @Test
    void testAddPlayerList(){
        AddPlayerlistModel listPlayer = new AddPlayerlistModel();


        AddPlayerModel player1 = new AddPlayerModel();
        player1.setForename("Zara");
        player1.setSurname("Marks");
        player1.setMail("zara.marks@reseau.eseo.fr");
        player1.setClub("Molkky Angers");

        AddPlayerModel player2 = new AddPlayerModel();
        player2.setForename("Aurélien");
        player2.setSurname("Masson");
        player2.setMail("aurelien.masson@reseau.eseo.fr");
        player2.setClub("Molkky Angers");

        listPlayer.addPlayer(player1);
        listPlayer.addPlayer(player2);

        Assertions.assertFalse(listPlayer.getPlayers().isEmpty(), "List Empty");
        Assertions.assertEquals(2,listPlayer.getPlayers().size(), "Size of List");
        Assertions.assertEquals("Marks",listPlayer.getPlayers().get(0).getSurname(), "Wrong player in place 1");
        Assertions.assertEquals("Masson",listPlayer.getPlayers().get(1).getSurname(), "Wrong player in place 2");

    }
}
