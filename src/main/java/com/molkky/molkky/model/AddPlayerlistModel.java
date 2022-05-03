package com.molkky.molkky.model;



import java.util.ArrayList;
import java.util.List;


public class AddPlayerlistModel {

    private List<AddPlayerModel> players ;

    public AddPlayerlistModel(){
        this.players = new ArrayList<>();
    }
    public void addPlayer(AddPlayerModel player){

        this.players.add(player);

    }

    public List<AddPlayerModel> getPlayers(){
        return this.players;
    }

    public void setPlayers(List<AddPlayerModel> players){
        this.players = players;
    }
}
