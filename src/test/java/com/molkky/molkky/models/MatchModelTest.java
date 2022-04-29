package com.molkky.molkky.models;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.model.MatchModel;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class MatchModelTest {
    @Test
    void testDefaultConstructor() {
        MatchModel matchModel = new MatchModel();
        Assertions.assertNotNull(matchModel);
    }

    @Test
    void testMatchConstructor(){
        Match match = new Match();
        match.setNbSets(3);
        match.setFinished(true);
        match.setId(1);
        MatchModel matchModel = new MatchModel(match);
        Assertions.assertEquals(3, matchModel.getNbSets());
        Assertions.assertTrue(matchModel.getFinished());
        Assertions.assertEquals(1, matchModel.getId());
    }
}
