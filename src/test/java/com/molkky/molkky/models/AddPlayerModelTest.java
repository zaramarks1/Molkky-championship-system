package com.molkky.molkky.models;

import type.UserRole;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.AddPlayerModel;
import com.molkky.molkky.model.AddPlayerlistModel;
import org.junit.Assert;
import org.junit.Test;

public class AddPlayerModelTest {

    @Test
    public void testAddPlayerConstructor1(){
        AddPlayerModel player = new AddPlayerModel("Marks","Zara","Molkky Angers","zara.marks@reseau.eseo.fr");

        Assert.assertEquals("Surname different", "Marks",player.getSurname());
        Assert.assertEquals("Forename different","Zara",player.getForename());
        Assert.assertEquals("Club different","Molkky Angers",player.getClub());
        Assert.assertEquals("Mail different","zara.marks@reseau.eseo.fr",player.getMail());
    }

    @Test
    public void testAddPlayerConstructor2(){
        AddPlayerModel player = new AddPlayerModel("Marks","Zara","Molkky Angers","zara.marks@reseau.eseo.fr",1);

        Assert.assertEquals("Id club different",Integer.valueOf(1),player.getTeamId());
    }

    @Test
    public void testAddPlayerFunction(){
        AddPlayerModel player = new AddPlayerModel("Marks","Zara","Molkky Angers","zara.marks@reseau.eseo.fr");
        User user = player.addPlayer();

        Assert.assertEquals("Surname different", player.getSurname(),user.getSurname());
        Assert.assertEquals("Forname different",player.getForename(),user.getForename());
        Assert.assertEquals("Club different",player.getClub(),user.getClub());
        Assert.assertEquals("Mail different",player.getMail(),user.getEmail());
        Assert.assertEquals("Role different", UserRole.PLAYER,user.getRole());
        Assert.assertFalse("User is registered",user.getIsRegistered());
        Assert.assertNotNull("Code not null",user.getCode());
    }

    @Test
    public void testCreateCodeLength(){
        AddPlayerModel player = new AddPlayerModel("Marks","Zara","Molkky Angers","zara.marks@reseau.eseo.fr");
        String code = player.createCode(10);

        Assert.assertEquals("Length not good",10,code.length());
    }

    @Test
    public void testAddPlayerList(){
        AddPlayerlistModel listPlayer = new AddPlayerlistModel();
        AddPlayerModel player1 = new AddPlayerModel("Marks","Zara","Molkky Angers","zara.marks@reseau.eseo.fr");
        AddPlayerModel player2 = new AddPlayerModel("Masson","Aurélien","Molkky Angers","aurelien.masson@reseau.eseo.fr");

        listPlayer.addPlayer(player1);
        listPlayer.addPlayer(player2);

        Assert.assertFalse("List Empty",listPlayer.getPlayers().isEmpty());
        Assert.assertEquals("Size of List",2,listPlayer.getPlayers().size());
        Assert.assertEquals("Wrong player in place 1","Marks",listPlayer.getPlayers().get(0).getSurname());
        Assert.assertEquals("Wrong player in place 2","Masson",listPlayer.getPlayers().get(1).getSurname());

    }
}
