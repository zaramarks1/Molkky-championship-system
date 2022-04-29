package com.molkky.molkky.models;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Set;
import com.molkky.molkky.model.SetModel;
import com.molkky.molkky.repository.SetRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.Arrays;
import java.util.List;

@SpringBootTest(classes = MolkkyApplication.class)
class SetModelTest {
    @Autowired
    SetRepository setRepository;


    @Test
    void testSetModelDefaultConst() {
        SetModel setModel = new SetModel();
        Assertions.assertNotNull(setModel);
    }

    @Test
    void testSetModelSet() {
        Set set = new Set();
        set.setFinished(true);
        set.setScoreTeam1(5);
        set.setScoreTeam2(5);
        set = setRepository.save(set);
        SetModel setModel = new SetModel(set);
        Assertions.assertNotNull(set.getId());
        Assertions.assertEquals(set.getFinished(), setModel.getFinished());
        Assertions.assertEquals(set.getScoreTeam1(), setModel.getScoreTeam1());
        Assertions.assertEquals(set.getScoreTeam2(), setModel.getScoreTeam2());
    }

    @Test
    void testSetModelSetMultiple() {
        List<Set> sets =  Arrays.asList(new Set(), new Set(), new Set());
        List<SetModel> models = SetModel.createSetModels(sets);
        Assertions.assertEquals(3, models.size());
    }
}
