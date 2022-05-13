package com.molkky.molkky.models;

import com.molkky.molkky.domain.Round;
import com.molkky.molkky.model.RoundModel;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import type.PhaseType;
class RoundModelTest {
    @Test
    void testDefaultConstructor() {
        RoundModel roundModel = new RoundModel();
        Assertions.assertNotNull(roundModel);
    }

    @Test
    void testRoundConstructor(){
        Round round = new Round();
        round.setId(1);
        round.setType(PhaseType.POOL);
        round.setFinished(true);
        RoundModel roundModel = new RoundModel(round);
        Assertions.assertNotNull(roundModel);
        Assertions.assertEquals(1, roundModel.getId());
        Assertions.assertEquals(PhaseType.POOL, roundModel.getType());
        Assertions.assertTrue(roundModel.getFinished());
    }
}
