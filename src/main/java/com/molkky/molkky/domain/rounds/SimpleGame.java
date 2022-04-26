package com.molkky.molkky.domain.rounds;

import Type.RoundType;
import com.molkky.molkky.domain.Round;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;

@Getter
@Entity
@Setter
@DiscriminatorValue("SimpleGame")
public class SimpleGame extends Round{
    public SimpleGame(){
        this.setType(RoundType.SIMPLEGAME);
    }

}
