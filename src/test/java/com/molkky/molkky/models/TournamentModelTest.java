package com.molkky.molkky.models;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Court;
import com.molkky.molkky.repository.CourtRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.text.ParseException;


class TournamentModelTest {

    @Test
    void testAddDate() throws ParseException {
        TournamentModel tournamentModel = new TournamentModel();
        tournamentModel.setDate("2019-01-01");
        Assertions.assertEquals("Tue Jan 01 00:00:00 CET 2019", tournamentModel.getDate().toString());
        tournamentModel.setCutOffDate("2020-01-01");
        Assertions.assertEquals("Wed Jan 01 00:00:00 CET 2020", tournamentModel.getCutOffDate().toString());
    }
}
