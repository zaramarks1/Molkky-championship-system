package com.molkky.molkky.domain.rounds;

import com.molkky.molkky.domain.Phase;
import type.PhaseType;
import com.molkky.molkky.domain.Round;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;

@Getter
@Entity
@Setter
@DiscriminatorValue("SIMPLEGAME")
public class SimpleGame extends Phase {
    public SimpleGame(){

    }

}
