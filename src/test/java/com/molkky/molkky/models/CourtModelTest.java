package com.molkky.molkky.models;

import com.molkky.molkky.domain.Court;
import com.molkky.molkky.model.CourtModel;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

class CourtModelTest {
    @Test
    void testDefaultConstructor(){
        CourtModel courtModel = new CourtModel();
        Assertions.assertNotNull(courtModel);
    }

    @Test
    void testCourtConstructor(){
        Court court = new Court();
        court.setAvailable(true);
        court.setId(1);
        court.setName("Court 1");
        CourtModel courtModel = new CourtModel(court);
        Assertions.assertNotNull(courtModel);
        Assertions.assertEquals(court.getId(), courtModel.getId());
        Assertions.assertEquals(court.getName(), courtModel.getName());
        Assertions.assertEquals(court.isAvailable(), courtModel.isAvailable());
    }

    @Test
    void testCourtConstructorMultiple(){
        List<Court> courts = new ArrayList<>();
        Court court = new Court();
        court.setAvailable(true);
        court.setId(1);
        court.setName("Court 1");
        courts.add(court);
        court = new Court();
        court.setAvailable(true);
        court.setId(2);
        court.setName("Court 2");
        courts.add(court);
        List<CourtModel> courtModels = CourtModel.createCourtModels(courts);
        Assertions.assertEquals(courts.size(), courtModels.size());
    }
}
