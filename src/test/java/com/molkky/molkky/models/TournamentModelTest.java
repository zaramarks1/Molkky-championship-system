package com.molkky.molkky.models;

import com.molkky.molkky.model.TournamentModel;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.text.ParseException;
import java.time.ZoneId;


class TournamentModelTest {

    @Test
    void testAddDate() throws ParseException {
        TournamentModel tournamentModel = new TournamentModel();
        tournamentModel.setDate("2019-01-01");
        Assertions.assertEquals("2019-01-01T00:00+01:00[Europe/Paris]",
                tournamentModel.getDate().toInstant().atZone(ZoneId.of("Europe/Paris")).toString()
        );
        tournamentModel.setCutOffDate("2020-01-01");
        Assertions.assertEquals("2020-01-01T00:00+01:00[Europe/Paris]",
                tournamentModel.getCutOffDate().toInstant().atZone(ZoneId.of("Europe/Paris")).toString()
        );
    }

    @Test
    void testWrongDates() throws ParseException {
        TournamentModel tournamentModel = new TournamentModel();
        tournamentModel.setDate("2019-01-01");
        tournamentModel.setCutOffDate("2020-01-01");
        Assertions.assertFalse(tournamentModel.isCutoffDateBeforeDate());
    }
}
