package com.molkky.molkky.models;

import com.molkky.molkky.domain.Club;
import com.molkky.molkky.model.ClubModel;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

class ClubModelTest {
    @Test
    void testDefaultConstructor(){
        ClubModel clubModel = new ClubModel();
        Assertions.assertNotNull(clubModel);
    }

    @Test
    void testClubConstructor(){
        Club club = new Club();
        club.setId(1);
        club.setName("Club 1");
        ClubModel clubModel = new ClubModel(club);
        Assertions.assertNotNull(clubModel);
        Assertions.assertEquals(club.getId(), clubModel.getId());
        Assertions.assertEquals(club.getName(), clubModel.getName());
    }

    @Test
    void testCourtConstructorMultiple(){
        List<Club> clubs = new ArrayList<>();
        Club club = new Club();
        club.setId(1);
        club.setName("Court 1");
        clubs.add(club);
        Club club2 = new Club();
        club2.setId(2);
        club2.setName("Court 2");
        clubs.add(club2);
        List<ClubModel> clubModels = ClubModel.createClubModels(clubs);
        Assertions.assertEquals(clubs.size(), clubModels.size());
    }
}
